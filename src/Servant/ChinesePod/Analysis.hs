-- | Analyze the vocab lists and try to match them against dialogues
--
-- This module is intended for use in @ghci@.
module Servant.ChinesePod.Analysis (
  --   initState
  -- , globalAnalysisState
  -- , getStats
    -- TODO: export list
    module Servant.ChinesePod.Analysis
    -- Re-exports
  , module Servant.ChinesePod.Vocab
  , module Servant.ChinesePod.HSK.HSK2012
  , module Data.IORef
  ) where

import Prelude hiding (Word, words)
import Control.Monad
import Data.Binary (Binary, encodeFile, decodeFile)
import Data.IORef
import Data.List (intercalate, sortBy, partition, nub, isInfixOf)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (PrettyVal, dumpStr)
import qualified Data.Map as Map hiding ((!))
import qualified Data.Set as Set

import Servant.ChinesePod.Vocab
import Servant.ChinesePod.HSK.HSK2012

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | Word in simplified characters
type Simpl = String

-- | The static part of the analysis (that doesn't change over time)
data AnalysisStatic = AnalysisStatic {
      -- | All lessons available
      analysisAllLessons :: Map V3Id Lesson

      -- | All words we're studying
    , analysisAllWords :: [Word]

      -- | "Inverse" index: which lessons cover a certain word?
      --
      -- We do not distinguish between key and supplemental vocabulary here,
      -- but only consider supplemental vocabulary that appears in the dialog.
      -- See comments for `RelevantLesson`.
    , analysisInverse :: Map Simpl [V3Id]
    }
  deriving (Generic, Show)

-- | The dynamic part of the analysis: words we've covered, lesosns we picked
data AnalysisDynamic = AnalysisDynamic {
      -- | Words we have left to chose lessons for
      analysisTodo :: [Word]

      -- | Lessons we've already chosen
      --
      -- Note that the concept of 'relevancy' is stateful: as we pick lessons
      -- and cover more words, fewer and fewer words are "relevant" (still to
      -- be covered). The relevancy information we record in 'analyisPicked'
      -- reflects this statefulness: we record the lessons in the order we
      -- choose them, and record the relevancy _at the time of choosing_.
    , analysisPicked :: [(V3Id, RelevantLesson)]

      -- | Lessons still available (that are relevant to 'analysisTodo')
      --
      -- This set will shrink as we remove words from 'analysisTodo'.
      -- We may also remove lessons from 'analysisAvailable' because they
      -- were explicitly filtered out (so we cannot reconstruct this from
      -- the other data).
    , analysisAvailable :: Map V3Id RelevantLesson
    }
  deriving (Generic, Show)

type AnalysisState = (AnalysisStatic, AnalysisDynamic)

-- | Lesson relevant to the set of words we are studying
--
-- A lesson is "relevant" if
--
-- > not (null rel)
--
-- We do not distinguish between key and supplemental vocabulary here; it is
-- too difficult to cover the HSK levels using only key vocabulary. However,
-- we consider _only_ the supplemental vocabulary that actually appears in the
-- dialogue. Vocabulary that does not appear in the dialogue does not help with
-- studying for the HSK, but does not harm either. Hence, we just ignore it.
data RelevantLesson = RelevantLesson {
      -- | Relevant vocabulary
      rel :: [Word]

      -- | Irrelevant vocabulary
      --
      -- We also record the HSK levels they appear in. This is useful for
      -- filtering/zooming.
    , irrel :: [(Word, [HSKLevel])]
    }
  deriving (Generic, Show)

instance PrettyVal AnalysisStatic
instance PrettyVal AnalysisDynamic
instance PrettyVal RelevantLesson

instance Binary AnalysisStatic
instance Binary AnalysisDynamic
instance Binary RelevantLesson

globalAnalysisState :: IORef AnalysisState
{-# NOINLINE globalAnalysisState #-}
globalAnalysisState = unsafePerformIO $ newIORef undefined

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
initRelevant words = Map.mapMaybe relevantLesson
  where
    relevantLesson :: Lesson -> Maybe RelevantLesson
    relevantLesson Lesson{..} = do
      let (rel, irrel') = partition isRelevantWord (key ++ supDialog)
          irrel         = map withLevel irrel'
      guard $ not (null rel)
      return RelevantLesson{..}

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

{-------------------------------------------------------------------------------
  Query the state
-------------------------------------------------------------------------------}

-- | Information about a lesson
data LessonSummary = LessonSummary {
      lessonSummaryId    :: V3Id
    , lessonSummaryTitle :: String
    , lessonSummaryLevel :: Level
    , lessonSummaryRel   :: [Word]

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

summarizeLesson :: V3Id -> (Lesson, RelevantLesson) -> IO LessonSummary
summarizeLesson lessonId (Lesson{..}, RelevantLesson{..}) = do
    allHarmless <- getHarmless

    let isHarmless :: Word -> Bool
        isHarmless w = source w `Set.member` allHarmless

        (harmless, notHarmless) = partition (isHarmless . fst) irrel

    return LessonSummary {
        lessonSummaryId       = lessonId
      , lessonSummaryTitle    = title
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

getInFocusAvailable :: IO [(V3Id, (Lesson, RelevantLesson))]
getInFocusAvailable = do
    (AnalysisStatic{analysisAllLessons}, AnalysisDynamic{analysisAvailable}) <- readIORef globalAnalysisState
    inFocus <- getFocus

    let pairLesson :: V3Id -> RelevantLesson -> (Lesson, RelevantLesson)
        pairLesson lId relevant = (analysisAllLessons `mapAt` lId, relevant)

    return $ Map.toList
           $ Map.filter inFocus
           $ Map.mapWithKey pairLesson
           $ analysisAvailable

getPicked :: IO [(V3Id, (Lesson, RelevantLesson))]
getPicked = do
    (AnalysisStatic{analysisAllLessons}, AnalysisDynamic{analysisPicked}) <- readIORef globalAnalysisState

    let pairLesson :: V3Id -> RelevantLesson -> (Lesson, RelevantLesson)
        pairLesson lId relevant = (analysisAllLessons `mapAt` lId, relevant)

    return $ mapWithKey pairLesson
           $ analysisPicked

-- | Show a readable summary of what's left to do
summarizeUsing :: (Ord a, Ord b) =>
                  (LessonSummary -> a) -- Sort key for lessons
               -> (WordSummary   -> b) -- Sort key forwords
               -> IO ()
summarizeUsing lessonKey wordKey = do
    (_static, AnalysisDynamic{analysisTodo}) <- readIORef globalAnalysisState

    available <- mapM (uncurry summarizeLesson) =<< getInFocusAvailable
    picked    <- mapM (uncurry summarizeLesson) =<< getPicked
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

    available <- mapM (uncurry summarizeLesson) =<< getInFocusAvailable

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

    let isMatching :: (V3Id, Lesson)
                   -> Maybe (V3Id, Lesson, RelevantLesson, String)
        isMatching (v3id, lesson) = do
          guard $ any (\word' -> word `isInfixOf` source word') (key lesson ++ supDialog lesson)
          let mAlreadyPicked      = lookup v3id analysisPicked
              mAvailable          = Map.lookup v3id analysisAvailable
              (relevant, comment) = case (mAlreadyPicked, mAvailable) of
                (Just alreadyPicked, _) -> (alreadyPicked, "already picked")
                (_, Just available)     -> (available, "available")
                (_, _)                  -> (irrelevantLesson lesson, "other")
          guard $ inFocus (lesson, relevant)
          return (v3id, lesson, relevant, comment)

    let matchingLessons :: [(V3Id, Lesson, RelevantLesson, String)]
        matchingLessons = mapMaybe isMatching (Map.toList analysisAllLessons)

    summarized <- forM matchingLessons $ \(v3id, lesson, relevant, comment) -> do
      summary <- summarizeLesson v3id (lesson, relevant)
      return (summary, comment)

    putStrLn "This word appears in the following in-focus lessons:"
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

type LessonPredicate = (Lesson, RelevantLesson) -> Bool

atLevel :: [Level] -> LessonPredicate
atLevel ls (Lesson{..}, _) = level `elem` ls

-- | Only allow irrelevant words from the specified HSK levels
irrelInHSK :: [HSKLevel] -> LessonPredicate
irrelInHSK ls (_, RelevantLesson{..}) = all (any (`elem` ls) . snd) irrel

-- | Maximum number of irrelevant words
maxNumIrrel :: Int -> LessonPredicate
maxNumIrrel n (_, RelevantLesson{..}) = length irrel <= n

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
        p' lessonId relevant = p (analysisAllLessons `mapAt` lessonId, relevant)

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

type HSKLevel = Int

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
showHskSplits = putStrLn . dumpStr . hskSplits

searchHsk :: String -> IO ()
searchHsk str = putStrLn . dumpStr
              $ filter matches
              $ concat
              $ Map.elems hskIndex
  where
    matches :: (HSKLevel, Word) -> Bool
    matches (_, Word{..}) = or [
        str `isInfixOf` pinyin
      , str `isInfixOf` source
      , str `isInfixOf` target
      ]

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

{-------------------------------------------------------------------------------
  Export
-------------------------------------------------------------------------------}

exportPleco :: String -> String -> IO ()
exportPleco fileName topCategory = do
    picked <- getPicked

    withFile fileName AppendMode $ \h ->
      forM_ (sortBy (comparing sortKey) picked) $
        \(_v3Id, (Lesson{..}, RelevantLesson{..})) -> do
          hPutStrLn h $ "//" ++ topCategory ++ "/" ++ title ++ " (" ++ dumpStr level ++ ")"
          forM_ (rel ++ map fst irrel) $ hPutStrLn h . source
  where
    -- Sort by level, then by ID
    sortKey :: (V3Id, (Lesson, RelevantLesson)) -> (Level, V3Id)
    sortKey (v3id, (Lesson{..}, _)) = (level, v3id)

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
