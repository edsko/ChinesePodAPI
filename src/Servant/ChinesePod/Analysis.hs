-- | Analyze the vocab lists and try to match them against dialogues
--
-- This module is intended for use in @ghci@.
module Servant.ChinesePod.Analysis (
    -- TODO: export list
    module Servant.ChinesePod.Analysis
    -- Re-exports
  , module State
  , module Vocab
  , module HSK
  , module Data.IORef
  ) where

import Prelude hiding (Word, words)
import Control.Monad
import Data.Bifunctor (first)
import Data.Binary (encodeFile, decodeFile)
import Data.IORef
import Data.List (intercalate, sortBy, partition, nub, isInfixOf)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Time
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import System.Process (callProcess)
import Text.Show.Pretty (PrettyVal, dumpStr)
import Text.Printf (printf)
import qualified Data.Map as Map hiding ((!))
import qualified Data.Set as Set

import Servant.ChinesePod.Analysis.State.V2           as State
import Servant.ChinesePod.HSK.HSK2012                 as HSK
import Servant.ChinesePod.Vocab.V2                    as Vocab
import qualified Servant.ChinesePod.API               as API
import qualified Servant.ChinesePod.Analysis.State.V1 as V1
import qualified Servant.ChinesePod.Analysis.State.V2 as V2

{-------------------------------------------------------------------------------
  Global state
-------------------------------------------------------------------------------}

globalAnalysisState :: IORef AnalysisState
{-# NOINLINE globalAnalysisState #-}
globalAnalysisState = unsafePerformIO $ newIORef (error "state not loaded")

updateAnalysisState' :: (AnalysisStatic -> AnalysisDynamic -> (AnalysisDynamic, b))
                    -> IO b
updateAnalysisState' f =
    atomicModifyIORef globalAnalysisState $ \(static, dynamic) ->
      first (static,) (f static dynamic)

updateAnalysisState :: (AnalysisStatic -> AnalysisDynamic -> AnalysisDynamic)
                    -> IO ()
updateAnalysisState f = modifyIORef globalAnalysisState $ \(static, dynamic) ->
    (static, f static dynamic)

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initState :: FilePath -> [Word] -> IO ()
initState vocabFile words = do
    vocab <- loadVocab vocabFile
    let static  = analysisStatic vocab words
        dynamic = analysisDynamic static
    writeIORef globalAnalysisState (static, dynamic)

-- | Reset the list of available lessons
--
-- Useful when we downloaded new lessons.
-- NOTE: This /only/ updates the static part of the state.
resetVocab :: FilePath -> IO ()
resetVocab vocabFile = do
    vocab <- loadVocab vocabFile
    modifyIORef globalAnalysisState $ \(static, dynamic) ->
      (analysisStatic vocab (analysisAllWords static), dynamic)

analysisStatic :: Vocab -> [Word] -> AnalysisStatic
analysisStatic (Vocab vocab) words = AnalysisStatic {
      analysisAllLessons = vocab
    , analysisAllWords   = words
    , analysisInverse    = computeInverse vocab words'
    }
  where
    words' :: [Simpl]
    words' = map source words

-- | Initial dynamic analysis state
analysisDynamic :: AnalysisStatic -> AnalysisDynamic
analysisDynamic AnalysisStatic{..} = AnalysisDynamic{..}
  where
    analysisTodo      = analysisAllWords
    analysisPicked    = []
    analysisAvailable = initRelevant (simplSet analysisTodo) analysisAllLessons

-- | Initial list of relevant lessons
initRelevant :: Set Simpl
             -> Map V3Id Lesson
             -> Map V3Id RelevantLesson
initRelevant = Map.mapMaybe . mkRelevant (not . null)

mkRelevant :: ([Word] -> Bool)  -- ^ Condition on the set of relevant words
           -> Set Simpl         -- ^ Words of interest
           -> Lesson -> Maybe RelevantLesson
mkRelevant checkRel words Lesson{..} = do
    let (rel, irrel') = partition isRelevantWord (key ++ supDialog)
        irrel         = map withLevel irrel'
    guard $ checkRel rel
    return RelevantLesson{..}
  where
    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = source `Set.member` words

simplSet :: [Word] -> Set Simpl
simplSet = Set.fromList . map source

-- | Cull relevant lessons after we've picked a lesson and so covered new words
--
-- We remove these words from the 'relevant' list because they are no longer
-- relevant (and so should no longer count towards how good a choice a lesson
-- is), but we don't add them to the irrelevant words because they should also
-- not count towards how bad a choice a lesson is.
cullRelevant :: Set Simpl
             -> Map V3Id RelevantLesson
             -> Map V3Id RelevantLesson
cullRelevant newCovered = Map.mapMaybe relevantLesson
  where
    relevantLesson :: RelevantLesson -> Maybe RelevantLesson
    relevantLesson RelevantLesson{..} = do
      let rel' = filter isRelevantWord rel
      guard $ not (null rel')
      return RelevantLesson { rel = rel', .. }

    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = not (source `Set.member` newCovered)

computeInverse :: Map V3Id Lesson
               -> [Simpl]
               -> Map Simpl [V3Id]
computeInverse vocab = Map.fromList . map go
  where
    vocab' :: [(V3Id, Lesson)]
    vocab' = Map.toList vocab

    go :: Simpl -> (Simpl, [V3Id])
    go word = (word, mapMaybe (contains word) vocab')

    contains :: Simpl -> (V3Id, Lesson) -> Maybe V3Id
    contains simpl (v3id, lesson) = do
      guard $ simpl `elem` map source (key lesson ++ supDialog lesson)
      return v3id

{-------------------------------------------------------------------------------
  Saving/loading
-------------------------------------------------------------------------------}

save :: FilePath -> IO ()
save fp = encodeFile fp =<< readIORef globalAnalysisState

open :: FilePath -> IO ()
open fp = writeIORef globalAnalysisState =<< decodeFile fp

-- | Migration
openV1 :: FilePath -> IO ()
openV1 fp = writeIORef globalAnalysisState =<< (v1_to_v2 <$> decodeFile fp)
  where
    v1_to_v2 :: V1.AnalysisState -> V2.AnalysisState
    v1_to_v2 = migrate

{-------------------------------------------------------------------------------
  All available information about a lesson
-------------------------------------------------------------------------------}

data LessonInfo = LessonInfo {
    lessonId :: V3Id
  , lesson   :: Lesson
  , relevant :: RelevantLesson
  }

infoFromRelevant :: V3Id -> RelevantLesson -> IO LessonInfo
infoFromRelevant lessonId relevant = do
    (AnalysisStatic{analysisAllLessons}, _) <- readIORef globalAnalysisState
    return LessonInfo {
        lessonId = lessonId
      , lesson   = analysisAllLessons `mapAt` lessonId
      , relevant = relevant
      }

-- | Get LessonInfo for all available lessons
getAvailableInfo :: IO [LessonInfo]
getAvailableInfo = do
    (_, AnalysisDynamic{analysisAvailable}) <- readIORef globalAnalysisState
    mapM (uncurry infoFromRelevant) (Map.toList analysisAvailable)

-- | Get LessonInfo for currently picked lessons
getPickedInfo :: IO [LessonInfo]
getPickedInfo = do
    (_, AnalysisDynamic{analysisPicked}) <- readIORef globalAnalysisState
    mapM (uncurry infoFromRelevant) analysisPicked

{-------------------------------------------------------------------------------
  Query the state
-------------------------------------------------------------------------------}

-- | Information about a lesson
data LessonSummary = LessonSummary {
      lessonSummaryId       :: V3Id
    , lessonSummaryTitle    :: String
    , lessonSummaryIsVideo  :: Bool
    , lessonSummaryReleased :: LocalTime
    , lessonSummaryLevel    :: Level
    , lessonSummaryRel      :: [Word]

      -- | Irrelevant but harmless
    , lessonSummaryHarmless :: [(Word, [HSKLevel])]

      -- | Truly irrelevant
    , lessonSummaryIrrel :: [(Word, [HSKLevel])]
    }

data WordSummary = WordSummary {
      wordSummarySimpl     :: Simpl
    , wordSummaryAppearsIn :: [V3Id]
    }

showLessonSummary :: Bool           -- ^ Show harmless?
                  -> LessonSummary
                  -> String
showLessonSummary includeHarmless LessonSummary{..} = concat [
      v3IdString lessonSummaryId
    , " ("
    , lessonSummaryTitle
    , ", "
    , show $ lessonSummaryLevel
    , ", "
    , formatTime defaultTimeLocale "%F" lessonSummaryReleased
    , if lessonSummaryIsVideo then ", video" else ""
    , "): "
    , intercalate "," $ map source lessonSummaryRel
    , " vs "
    , intercalate "," $ map sourceWithLevel lessonSummaryIrrel
    , if includeHarmless
        then "/" ++ intercalate "," (map sourceWithLevel lessonSummaryHarmless)
        else ""
    , " ("
    , show $ length lessonSummaryRel
    , " vs "
    , show $ length lessonSummaryIrrel
    , if includeHarmless
        then "/" ++ show (length lessonSummaryHarmless)
        else ""
    , ")"
    ]
  where
    sourceWithLevel :: (Word, [HSKLevel]) -> String
    sourceWithLevel (Word{..}, levels) = source ++ show levels

showWordSummary :: WordSummary -> String
showWordSummary WordSummary{..} = concat [
      wordSummarySimpl
    , " ("
    , show $ length wordSummaryAppearsIn
    , ")"
    ]

summarizeLesson :: LessonInfo -> IO LessonSummary
summarizeLesson LessonInfo{ lessonId
                          , lesson   = Lesson{..}
                          , relevant = RelevantLesson{..}
                          } = do
    allHarmless <- getHarmless

    let isHarmless :: Word -> Bool
        isHarmless w = source w `Set.member` allHarmless

        (harmless, notHarmless) = partition (isHarmless . fst) irrel

    return LessonSummary {
        lessonSummaryId       = lessonId
      , lessonSummaryTitle    = title
      , lessonSummaryIsVideo  = isVideo
      , lessonSummaryReleased = released
      , lessonSummaryLevel    = level
      , lessonSummaryRel      = rel
      , lessonSummaryHarmless = harmless
      , lessonSummaryIrrel    = notHarmless
      }

summarizeWord :: Word -> IO WordSummary
summarizeWord Word{..} = do
    (AnalysisStatic{analysisInverse}, _dyn) <- readIORef globalAnalysisState

    return WordSummary {
        wordSummarySimpl     = source
      , wordSummaryAppearsIn = analysisInverse `mapAt` source
      }

getInFocusAvailable :: IO [LessonInfo]
getInFocusAvailable = do
    inFocus <- getFocus
    filter inFocus <$> getAvailableInfo

-- | Show a readable summary of what's left to do
summarizeUsing :: (Ord a, Ord b) =>
                  (LessonSummary -> a) -- Sort key for lessons
               -> (WordSummary   -> b) -- Sort key forwords
               -> IO ()
summarizeUsing lessonKey wordKey = do
    (_static, AnalysisDynamic{analysisTodo}) <- readIORef globalAnalysisState

    available <- mapM summarizeLesson =<< getInFocusAvailable
    picked    <- mapM summarizeLesson =<< getPickedInfo
    todo      <- mapM summarizeWord analysisTodo
    zoomedIn  <- isZoomedIn

    putStrLn $ "Available lessons (" ++ show (length available) ++ ")"
            ++ if zoomedIn then " (zoomed in)" else ""
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map (showLessonSummary False)
             $ sortBy (comparing lessonKey)
             $ available
    putStrLn $ ""

    putStrLn $ "Picked lessons (" ++ show (length picked) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n" $ map (showLessonSummary False) picked
    putStrLn $ ""

    putStrLn $ "Words left (" ++ show (length analysisTodo) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate ","
             $ map showWordSummary
             $ sortBy (comparing wordKey)
             $ todo
    putStrLn $ ""

-- | Show information about a given lesson
infoLesson :: V3Id -> IO ()
infoLesson lessonId = do
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState

    putStrLn . dumpStr $ analysisAllLessons `mapAt` lessonId

    forM_ (lookup lessonId analysisPicked) $ \relevantLesson -> do
      putStrLn "We picked this lesson:"
      putStrLn . dumpStr $ relevantLesson

    forM_ (Map.lookup lessonId analysisAvailable) $ \relevantLesson -> do
      putStrLn "This lesson is available:"
      putStrLn . dumpStr $ relevantLesson

-- | Show information about a given word
infoWord :: Simpl -> IO ()
infoWord source = do
    (AnalysisStatic{analysisInverse}, _dyn) <- readIORef globalAnalysisState

    let appearsIn :: [V3Id]
        appearsIn = analysisInverse `mapAt` source

    available <- mapM summarizeLesson =<< getInFocusAvailable

    putStrLn "This word is in the vocab of the following available lessons:"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map (showLessonSummary False)
             $ sortBy (comparing countLessonRelIrrel)
             $ filter ((`elem` appearsIn) . lessonSummaryId)
             $ available
    putStrLn $ ""

{-------------------------------------------------------------------------------
  Search vocabulary for a word
-------------------------------------------------------------------------------}

-- | Search for a word in the vocabulary of all lessons
--
-- (including this word appearing as a subword)
searchVocab :: Simpl -> IO ()
searchVocab word = do
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState
    inFocus <- getFocus

    let isMatching :: (V3Id, Lesson) -> Maybe (LessonInfo, String)
        isMatching (v3id, lesson) = do
          guard $ any (\word' -> word `isInfixOf` source word') (key lesson ++ supDialog lesson)
          let mAlreadyPicked      = lookup v3id analysisPicked
              mAvailable          = Map.lookup v3id analysisAvailable
              (relevant, comment) = case (mAlreadyPicked, mAvailable) of
                (Just alreadyPicked, _) -> (alreadyPicked, "already picked")
                (_, Just available)     -> (available, "available")
                (_, _)                  -> (irrelevantLesson lesson, "other")
              lessonInfo          = LessonInfo v3id lesson relevant
          guard $ inFocus lessonInfo
          return (lessonInfo, comment)

    let matchingLessons :: [(LessonInfo, String)]
        matchingLessons = mapMaybe isMatching (Map.toList analysisAllLessons)

    summarized <- forM matchingLessons $ \(lessonInfo, comment) -> do
      summary <- summarizeLesson lessonInfo
      return (summary, comment)

    putStrLn $ word ++ " appears in the following in-focus lessons:"
    putStrLn "-----------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map showWithComment
             $ sortBy (comparing (countLessonRelIrrel . fst))
             $ summarized
  where
    showWithComment :: (LessonSummary, String) -> String
    showWithComment (summary, comment) = showLessonSummary True summary
                                      ++ " (" ++ comment ++ ")"

irrelevantLesson :: Lesson -> RelevantLesson
irrelevantLesson Lesson{..} = RelevantLesson{
    rel   = []
  , irrel = map withLevel (key ++ supDialog)
  }

{-------------------------------------------------------------------------------
  Different kinds of sorting functions
-------------------------------------------------------------------------------}

-- | Number of relevant words in the vocabulary of the lesson
countLessonRel :: LessonSummary -> Int
countLessonRel LessonSummary{..} =
    length lessonSummaryRel

-- | Number of relevant and irrelevant words in the vocab of the lesson
--
-- We multiply the number of irrelevant words by -1, so that we sort them
-- in the opposite direction.
countLessonRelIrrel :: LessonSummary -> (Int, Int)
countLessonRelIrrel LessonSummary{..} =
    (length lessonSummaryRel, negate (length lessonSummaryIrrel))

-- | Number of lessons that have this word in their key vocabulary
countWordAppearsIn :: WordSummary -> Int
countWordAppearsIn WordSummary{..} =
    length wordSummaryAppearsIn

-- | Number of words in the irrelevant key vocabulary that are not in the
-- specified HSK levels
countReallyIrrelevant :: [HSKLevel] -> LessonSummary -> Int
countReallyIrrelevant ls LessonSummary{..} =
  length $ filter (none (`elem` ls) . snd) lessonSummaryIrrel

-- | Summarize using default sorting
summarize :: IO ()
summarize = summarizeUsing countLessonRelIrrel countWordAppearsIn

{-------------------------------------------------------------------------------
  Predicates on lessons
-------------------------------------------------------------------------------}

type LessonPredicate = LessonInfo -> Bool

atLevel :: [Level] -> LessonPredicate
atLevel ls LessonInfo{lesson = Lesson{..}} =
    level `elem` ls

-- | Only allow irrelevant words from the specified HSK levels
irrelInHSK :: [HSKLevel] -> LessonPredicate
irrelInHSK ls LessonInfo{relevant = RelevantLesson{..}} =
    all (any (`elem` ls) . snd) irrel

-- | Maximum number of irrelevant words
maxNumIrrel :: Int -> LessonPredicate
maxNumIrrel n LessonInfo{relevant = RelevantLesson{..}} =
    length irrel <= n

onlyLessons :: [V3Id] -> LessonPredicate
onlyLessons lessonIds LessonInfo{lessonId = lessonId} =
    lessonId `elem` lessonIds

{-------------------------------------------------------------------------------
  Operations on the dynamic state
-------------------------------------------------------------------------------}

filterLessons :: LessonPredicate -> IO ()
filterLessons p = updateAnalysisState aux
  where
    aux :: AnalysisStatic -> AnalysisDynamic -> AnalysisDynamic
    aux AnalysisStatic{..} AnalysisDynamic{..} = AnalysisDynamic{
          analysisAvailable = Map.filterWithKey p' analysisAvailable
        , ..
        }
      where
        p' :: V3Id -> RelevantLesson -> Bool
        p' lessonId relevant = p LessonInfo{
            lessonId = lessonId
          , lesson   = analysisAllLessons `mapAt` lessonId
          , relevant = relevant
          }

-- | Remove a lesson from the available set
dropLesson :: V3Id -> IO ()
dropLesson lessonId' = filterLessons notSpecifiedLesson
  where
    notSpecifiedLesson :: LessonPredicate
    notSpecifiedLesson LessonInfo{lessonId} = lessonId /= lessonId'

-- | Pick a lesson
pick :: V3Id -> IO ()
pick lessonId = updateAnalysisState aux
  where
    aux :: AnalysisStatic -> AnalysisDynamic -> AnalysisDynamic
    aux AnalysisStatic{..} AnalysisDynamic{..} = AnalysisDynamic{
          analysisTodo      = analysisTodo'
        , analysisPicked    = analysisPicked'
        , analysisAvailable = analysisAvailable'
        }
      where
        analysisTodo'      = filter (not . inLesson) analysisTodo
        analysisPicked'    = analysisPicked ++ [(lessonId, pickedLesson)]
        analysisAvailable' = cullRelevant pickedLessonWords $
                               Map.delete lessonId analysisAvailable

        inLesson :: Word -> Bool
        inLesson Word{..} = source `Set.member` pickedLessonWords

        pickedLesson :: RelevantLesson
        pickedLesson = case Map.lookup lessonId analysisAvailable of
          Just relevant -> relevant
          Nothing       -> irrelevantLesson (analysisAllLessons `mapAt` lessonId)

        pickedLessonWords :: Set Simpl
        pickedLessonWords = simplSet $ rel pickedLesson

-- | Remove a word from TODO without actually covering it
skip :: Simpl -> IO ()
skip simpl = updateAnalysisState aux
  where
    aux :: AnalysisStatic -> AnalysisDynamic -> AnalysisDynamic
    aux _ AnalysisDynamic{..} = AnalysisDynamic{
          analysisTodo      = analysisTodo'
        , analysisPicked    = analysisPicked
        , analysisAvailable = analysisAvailable'
        }
      where
        analysisTodo'      = filter ((/= simpl) . source) analysisTodo
        analysisAvailable' = cullRelevant (Set.singleton simpl)
                               analysisAvailable

-- | Add a word back into TODO
--
-- NOTE: This may reintroduce lessons into 'analysisAvailable' that were
-- previously explicitly removed.
unskip :: Simpl -> IO ()
unskip simpl = updateAnalysisState aux
  where
    aux :: AnalysisStatic -> AnalysisDynamic -> AnalysisDynamic
    aux AnalysisStatic{..} AnalysisDynamic{..} = AnalysisDynamic{
          analysisTodo      = newTodo ++ analysisTodo
        , analysisPicked    = analysisPicked
        , analysisAvailable = Map.union newAvailable analysisAvailable
        }
      where
        newTodo      = filter ((== simpl) . source) analysisAllWords
        newAvailable = initRelevant (Set.singleton simpl) analysisAllLessons

{-------------------------------------------------------------------------------
  Focusing on a subset
-------------------------------------------------------------------------------}

-- | Add an additional constraint on the focus
zoomIn :: LessonPredicate -> IO ()
zoomIn p = modifyIORef globalFocus $ FocusZoom p

-- | Undo one 'zoomIn'
zoomOut :: IO ()
zoomOut = modifyIORef globalFocus go
  where
    go :: Focus -> Focus
    go FocusAll         = FocusAll
    go (FocusZoom _ ps) = ps

-- | Focus only on lessons satisfying the specified predciate
setFocus :: LessonPredicate -> IO ()
setFocus = writeIORef globalFocus . predicateToFocus
  where
    predicateToFocus :: LessonPredicate -> Focus
    predicateToFocus p = FocusZoom p FocusAll

-- | Get current focus as a predicate
getFocus :: IO LessonPredicate
getFocus = focusToPredicate <$> readIORef globalFocus
  where
    focusToPredicate :: Focus -> LessonPredicate
    focusToPredicate FocusAll         _ = True
    focusToPredicate (FocusZoom p ps) l = p l && focusToPredicate ps l

-- | Zoom all the way out
resetFocus :: IO ()
resetFocus = writeIORef globalFocus FocusAll

-- | Are we applying some predicates?
isZoomedIn :: IO Bool
isZoomedIn = do
    focus <- readIORef globalFocus
    case focus of
      FocusAll   -> return False
      _otherwise -> return True

data Focus =
    -- | Show all available lessons
    FocusAll

    -- | Limit the focus
  | FocusZoom LessonPredicate Focus

globalFocus :: IORef Focus
{-# NOINLINE globalFocus #-}
globalFocus = unsafePerformIO $ newIORef FocusAll

{-------------------------------------------------------------------------------
  HSK level for each word
-------------------------------------------------------------------------------}

hskLevel :: Simpl -> [(HSKLevel, Word)]
hskLevel simpl = Map.findWithDefault [] simpl hskIndex

hskIndex :: Map Simpl [(HSKLevel, Word)]
hskIndex = Map.unionsWith (++) [
      indexFor 1 hsk1
    , indexFor 2 hsk2
    , indexFor 3 hsk3
    , indexFor 4 hsk4
    , indexFor 5 hsk5
    , indexFor 6 hsk6
    ]
  where
    indexFor :: HSKLevel -> [Word] -> Map Simpl [(HSKLevel, Word)]
    indexFor level = Map.fromList . map go
      where
        go :: Word -> (Simpl, [(HSKLevel, Word)])
        go word@Word{..} = (source, [(level, word)])

withLevel :: Word -> (Word, [HSKLevel])
withLevel w = (w, map fst . hskLevel . source $ w)

showHskLevel :: Simpl -> IO ()
showHskLevel = putStrLn . dumpStr . hskLevel

-- | Try to find split a word into smaller words which are in HSK levels
hskSplits :: Simpl -> [(HSKLevel, Word)]
hskSplits = nub . concatMap hskLevel . nub . concat . splits

showHskSplits :: Simpl -> IO ()
showHskSplits w = do
    putStrLn $ "* HSK splits for " ++ w
    putStrLn $ dumpStr (hskSplits w)

searchHsk :: String -> [(HSKLevel, Word)]
searchHsk str = filter matches
              $ concat
              $ Map.elems hskIndex
  where
    matches :: (HSKLevel, Word) -> Bool
    matches (_, Word{..}) = or [
        str `isInfixOf` pinyin
      , str `isInfixOf` source
      , str `isInfixOf` target
      ]

showSearchHsk :: String -> IO ()
showSearchHsk = putStrLn . dumpStr . searchHsk

{-------------------------------------------------------------------------------
  "Harmless" words are irrelevant words that don't really matter; for instance,
  a lesson might have 十一 in the vocab which isn't listed explicitly in any
  HSK, but shouldn't really count towards the number of irrelevant words in
  that lesson.
-------------------------------------------------------------------------------}

-- | Save the list of harmless words
--
-- We store the list of harmless words separately so we can load it independent
-- of the rest of the state
saveHarmless :: FilePath -> IO ()
saveHarmless fp = encodeFile fp =<< readIORef globalHarmless

-- | Load a list of harmless words
openHarmless :: FilePath -> IO ()
openHarmless fp = writeIORef globalHarmless =<< decodeFile fp

-- | Get current list of harmless words
getHarmless :: IO (Set Simpl)
getHarmless = readIORef globalHarmless

-- | Show current words marked as harmless
showHarmless :: IO ()
showHarmless = putStrLn . format . Set.toList =<< readIORef globalHarmless
  where
    format = intercalate ","

-- | Mark a word as harmless
markHarmless :: Simpl -> IO ()
markHarmless w = modifyIORef globalHarmless $ Set.insert w

globalHarmless :: IORef (Set Simpl)
{-# NOINLINE globalHarmless #-}
globalHarmless = unsafePerformIO $ newIORef Set.empty

-- | Given a lesson, analyze which irrelevant words may be harmless
--
-- This can be applied to relevant still-available lessons as well as lessons
-- we already picked (removing harmless irrelevant words from already picked
-- lessons will improve the statistics).
analyzeIrrelevant :: V3Id -> IO ()
analyzeIrrelevant lessonId = do
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState
    let lesson     = analysisAllLessons `mapAt` lessonId
        relevant   = case lookup lessonId analysisPicked of
                       Just rl -> rl
                       Nothing -> analysisAvailable  `mapAt` lessonId
        lessonInfo = LessonInfo lessonId lesson relevant
    summary <- summarizeLesson lessonInfo
    forM_ (lessonSummaryIrrel summary) $ \(word, levels) -> do
      putStrLn $ "* " ++ source word ++ " " ++ show levels
      let split     = hskSplits (source word)
          covered   = concatMap (source . snd) split
          uncovered = filter (`notElem` covered) (source word)
      putStrLn $ "Splits: " ++ showMatches split
      unless (null uncovered) $ do
        putStrLn $ "Uncovered: "
        forM_ uncovered $ \char -> do
          let searched = searchHsk [char]
          putStrLn $ [char] ++ ": " ++ showMatches searched
      putStrLn ""
  where
    showMatches :: [(HSKLevel, Word)] -> String
    showMatches = intercalate "," . map compact . sortBy (comparing fst)

    compact :: (HSKLevel, Word) -> String
    compact (level, word) = source word ++ "(" ++ show level ++ ")"

{-------------------------------------------------------------------------------
  Local optimization

  Consider each already picked lesson and see if it can be replaced by another,
  without replacing anything else. This is useful for instance when updating a
  list when new dialogues have been published.
-------------------------------------------------------------------------------}

type Improvement = RelevantLesson                       -- ^ Current choice
                -> [(V3Id, RelevantLesson)]             -- ^ Available choices
                -> Either String (V3Id, RelevantLesson) -- ^ New choice, if any

improveLocal :: Improvement -> IO [String]
improveLocal f = updateAnalysisState' aux
  where
    aux :: AnalysisStatic -> AnalysisDynamic -> (AnalysisDynamic, [String])
    aux AnalysisStatic{..} AnalysisDynamic{..} = ( AnalysisDynamic{
            analysisPicked    = newPicked
          , analysisAvailable = cullRelevant newCovered analysisAvailable
          , analysisTodo      = filter
                                  (\w -> source w `Set.notMember` newCovered)
                                  analysisTodo
          }
        , msgs
        )
      where
        newPicked :: [(V3Id, RelevantLesson)]
        msgs      :: [String]
        (newPicked, msgs) = unzip $ map tryPickNew analysisPicked

        newCovered :: Set Simpl
        newCovered = Set.fromList $ map source $ concatMap (rel . snd) newPicked

        tryPickNew :: (V3Id, RelevantLesson) -> ((V3Id, RelevantLesson), String)
        tryPickNew (oldId, old) =
            case f' (oldId, old) of
              Left  msg          -> ((oldId, old), msg)
              Right (newId, new) -> undefined -- TODO

        f' :: (V3Id, RelevantLesson) -> Either String (V3Id, RelevantLesson)
        f' (oldId, old) = f old (Map.toList $ Map.delete oldId candidates)
          where
            oldRel     = Set.fromList $ map source (rel old)
            allCovered = \newRel -> all (\w -> w `elem` newRel) (rel old)
            candidates = Map.mapMaybe
                           (mkRelevant allCovered oldRel)
                           analysisAllLessons

-- | For debugging: never suggest improvements, just show the candidates
dontImprove :: Improvement
dontImprove old candidates = Left $ show (old, candidates)

{-------------------------------------------------------------------------------
  Export
-------------------------------------------------------------------------------}

exportPleco :: String -> String -> IO ()
exportPleco = exportPleco' (const True)

exportPleco' :: LessonPredicate -> String -> String -> IO ()
exportPleco' p fileName topCategory = do
    picked <- filter p <$> getPickedInfo
    withFile fileName AppendMode $ \h ->
      forM_ (sortBy (comparing exportSortKey) picked) $
        \LessonInfo{lessonId = _v3Id, lesson = Lesson{..}, relevant = RelevantLesson{..}} -> do
          hPutStrLn h $ "//" ++ topCategory ++ "/" ++ title ++ " (" ++ dumpStr level ++ ")"
          forM_ (rel ++ map fst irrel) $ hPutStrLn h . source

exportMarkdown :: String -> String -> IO ()
exportMarkdown = exportMarkdown' (const True)

exportMarkdown' :: LessonPredicate -> String -> String -> IO ()
exportMarkdown' p fileName header = do
    picked <- filter p <$> getPickedInfo
    withFile fileName AppendMode $ \h -> do
      hPutStrLn h $ header
      hPutStrLn h $ replicate (length header) '-'
      forM_ (sortBy (comparing exportSortKey) picked) $
        \LessonInfo{lessonId = v3Id, lesson = Lesson{..}, relevant = RelevantLesson{..}} -> do
           content :: API.LessonContent <- decodeFile $ "content/" ++ v3IdString v3Id
           let url = "https://chinesepod.com/lessons/" ++ API.lessonContentSlug content
           hPutStrLn h $ "* [" ++ title ++ " (" ++ dumpStr level ++ ")](" ++ url ++ ")"

downloadAudio :: FilePath -> IO ()
downloadAudio = downloadAudio' (const True)

downloadAudio' :: LessonPredicate -> FilePath -> IO ()
downloadAudio' p dest = do
    picked <- filter p <$> getPickedInfo
    forM_ (zip [1..] (sortBy (comparing exportSortKey) picked)) $
      \(i, LessonInfo{lessonId = v3Id, lesson = Lesson{..}, relevant = RelevantLesson{..}}) -> do
         content :: API.LessonContent <- decodeFile $ "content/" ++ v3IdString v3Id
         let slug               = API.lessonContentSlug content
             Just mp3FullLesson = API.lessonContentCdQualityMp3 content
             pref               = printf "%03d" (i :: Int) ++ "-"
             filename           = pref ++ slug ++ ".mp3"
             pathLesson         = dest </> "lesson"   </> filename
             pathDialogue       = dest </> "dialogue" </> filename

         unlessFileExists pathLesson $
           callProcess "curl" [mp3FullLesson, "-o", pathLesson]

         case API.lessonContentDialogueMp3  content of
           Nothing ->
             return () -- QW don't have dialogues
           Just mp3Dialogue -> unlessFileExists pathDialogue $
             callProcess "curl" [mp3Dialogue, "-o", pathDialogue]


  where
    -- Technically, this has a race condition, but who cares
    unlessFileExists :: FilePath -> IO () -> IO ()
    unlessFileExists fp act = do
      exists <- doesFileExist fp
      if exists then putStrLn $ "Skipping " ++ fp
                else act

-- Sort by level, then by ID
exportSortKey :: LessonInfo -> (Level, V3Id)
exportSortKey LessonInfo{lessonId = v3id, lesson = Lesson{..}} = (level, v3id)

{-------------------------------------------------------------------------------
  Statistics
-------------------------------------------------------------------------------}

-- | Simple statistics
--
-- The "harmless" words are not inclided in the irrelevant statistics here.
data Stats = Stats {
      statsNumPicked  :: Int
    , statsNumRel     :: Int
    , statsNumIrrel   :: Int
    , statsRelVsIrrel :: Double
    }
  deriving (Generic)

instance PrettyVal Stats

printStats :: IO ()
printStats = printStats' (const True)

printStats' :: LessonPredicate -> IO ()
printStats' p = do
    picked   <- filter p <$> getPickedInfo
    harmless <- getHarmless

    let totalRel, totalIrrel :: Set Simpl
        totalRel    = Set.fromList $ concatMap getRel picked
        totalIrrel' = concatMap getIrrel picked
        totalIrrel  = Set.fromList $ filter (`notElem` harmless) totalIrrel'

        statsNumPicked  = length picked
        statsNumRel     = length totalRel
        statsNumIrrel   = length totalIrrel
        statsRelVsIrrel = (fromIntegral statsNumRel / fromIntegral (statsNumRel + statsNumIrrel)) * 100
    putStrLn $ dumpStr Stats{..}
  where
    getRel, getIrrel :: LessonInfo -> [Simpl]
    getRel   LessonInfo{relevant = RelevantLesson{..}} = map source rel
    getIrrel LessonInfo{relevant = RelevantLesson{..}} = map (source . fst) irrel

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mapWithKey :: forall k a b. (k -> a -> b) -> [(k,a)] -> [(k,b)]
mapWithKey f = map go
  where
    go :: (k,a) -> (k,b)
    go (k,a) = (k, f k a)

none :: (a -> Bool) -> [a] -> Bool
none p = not . any p

-- | All different ways to split a list into sublists (without reordering)
splits :: forall a. [a] -> [[[a]]]
splits []     = [[]]
splits [x]    = [[[x]]]
splits (x:xs) = map (\(ys:yss) -> (x:ys):yss) (splits xs)
             ++ map ([x] :)                   (splits xs)

mapAt :: (Ord k, Show k) => Map k a -> k -> a
mapAt mp k = case Map.lookup k mp of
               Just a  -> a
               Nothing -> error $ "Unknown key " ++ show k
