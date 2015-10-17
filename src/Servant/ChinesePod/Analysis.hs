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
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (PrettyVal, dumpStr)
import qualified Data.Map as Map
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
      -- For each word we record which lessons contain that word in their
      -- key vocabulary and which lessons contain that word in their
      -- supplemental vocabulary.
    , analysisInverse :: Map Simpl ([V3Id], [V3Id])
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
-- > not (null (relKey ++ relSup))
data RelevantLesson = RelevantLesson {
      -- | Relevant key vocabulary
      relKey :: [Word]

      -- | Relevant supplemental vocabulary
    , relSup :: [Word]

      -- | Irrelevant words in the key vocabulary
      --
      -- We also record the HSK levels they appear in. This is useful for
      -- filtering/zooming.
    , irrelKey :: [(Word, [HSKLevel])]

      -- | Irrelevant words in the supplemental vocabulary
    , irrelSup :: [(Word, [HSKLevel])]
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
      let (relKey, irrelKey') = partition isRelevantWord key
          (relSup, irrelSup') = partition isRelevantWord sup
          irrelKey = map withLevel irrelKey'
          irrelSup = map withLevel irrelSup'
      guard $ not (null (relKey ++ relSup))
      return RelevantLesson{..}

    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = source `Set.member` words

simplSet :: [Word] -> Set Simpl
simplSet = Set.fromList . map source

-- | Cull relevant lessons after we've picked a lesson and so covered new words
--
-- We remove these words from the 'relevant' list beacuse they are no longer
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
      let relKey' = filter isRelevantWord relKey
          relSup' = filter isRelevantWord relSup
      guard $ not (null (relKey' ++ relSup'))
      return RelevantLesson {
          relKey = relKey'
        , relSup = relSup'
        , ..
        }

    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = not (source `Set.member` newCovered)

computeInverse :: Map V3Id Lesson
               -> [Simpl]
               -> Map Simpl ([V3Id], [V3Id])
computeInverse vocab = Map.fromList . map go
  where
    vocab' :: [(V3Id, Lesson)]
    vocab' = Map.toList vocab

    go :: Simpl -> (Simpl, ([V3Id], [V3Id]))
    go word = ( word
              , ( mapMaybe (containsIn key word) vocab'
                , mapMaybe (containsIn sup word) vocab'
                )
              )

    containsIn :: (Lesson -> [Word]) -> Simpl -> (V3Id, Lesson) -> Maybe V3Id
    containsIn f simpl (v3id, lesson) = do
      guard $ simpl `elem` map source (f lesson)
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
      lessonSummaryId       :: V3Id
    , lessonSummaryTitle    :: String
    , lessonSummaryRelKey   :: [Word]
    , lessonSummaryRelSup   :: [Word]
    , lessonSummaryLevel    :: Level

      -- | Irrelevant words in the key vocab, along with their HSK level
      --
      -- We record the HSK level so we can judge how irrelevant they are. Also,
      -- this should not include any "harmless" words.
    , lessonSummaryIrrelKey :: [(Word, [HSKLevel])]

      -- | Irrelevant words in the sup vocab. See also 'lessonSummaryIrrelKey'.
    , lessonSummaryIrrelSup :: [(Word, [HSKLevel])]
    }

data WordSummary = WordSummary {
      wordSummarySimpl :: Simpl
    , wordSummaryInKey :: [V3Id]
    , wordSummaryInSup :: [V3Id]
    }

instance Show LessonSummary where
  show LessonSummary{..} = concat [
        v3IdString lessonSummaryId
      , " ("
      , lessonSummaryTitle
      , ", "
      , show $ lessonSummaryLevel
      , "): "
      , intercalate "," $ map source lessonSummaryRelKey
      , "/"
      , intercalate "," $ map source lessonSummaryRelSup
      , " vs "
      , intercalate "," $ map sourceWithLevel lessonSummaryIrrelKey
      , "/"
      , intercalate "," $ map sourceWithLevel lessonSummaryIrrelSup
      , " ("
      , show $ length lessonSummaryRelKey
      , "/"
      , show $ length lessonSummaryRelSup
      , " vs "
      , show $ length lessonSummaryIrrelKey
      , "/"
      , show $ length lessonSummaryIrrelSup
      , ")"
      ]
    where
      sourceWithLevel :: (Word, [HSKLevel]) -> String
      sourceWithLevel (Word{..}, levels) = source ++ show levels

instance Show WordSummary where
  show WordSummary{..} =  concat [
        wordSummarySimpl
      , " ("
      , show $ length wordSummaryInKey
      , "/"
      , show $ length wordSummaryInSup
      , ")"
      ]

summarizeLesson :: V3Id -> (Lesson, RelevantLesson) -> IO LessonSummary
summarizeLesson lessonId (Lesson{..}, RelevantLesson{..}) = do
    allHarmless <- getHarmless

    let notHarmless :: Word -> Bool
        notHarmless w = not (source w `Set.member` allHarmless)

    return LessonSummary {
        lessonSummaryId       = lessonId
      , lessonSummaryTitle    = title
      , lessonSummaryRelKey   = relKey
      , lessonSummaryRelSup   = relSup
      , lessonSummaryLevel    = level
      , lessonSummaryIrrelKey = filter (notHarmless . fst) irrelKey
      , lessonSummaryIrrelSup = filter (notHarmless . fst) irrelSup
      }

summarizeWord :: Word -> IO WordSummary
summarizeWord Word{..} = do
    (AnalysisStatic{analysisInverse}, _dyn) <- readIORef globalAnalysisState

    let inKey, inSup :: [V3Id]
        (inKey, inSup) = analysisInverse Map.! source

    return WordSummary {
        wordSummarySimpl = source
      , wordSummaryInKey = inKey
      , wordSummaryInSup = inSup
      }

getInFocusAvailable :: IO [(V3Id, (Lesson, RelevantLesson))]
getInFocusAvailable = do
    (AnalysisStatic{analysisAllLessons}, AnalysisDynamic{analysisAvailable}) <- readIORef globalAnalysisState
    inFocus <- getFocus

    let pairLesson :: V3Id -> RelevantLesson -> (Lesson, RelevantLesson)
        pairLesson lId relevant = (analysisAllLessons Map.! lId, relevant)

    return $ Map.toList
           $ Map.filter inFocus
           $ Map.mapWithKey pairLesson
           $ analysisAvailable

getPicked :: IO [(V3Id, (Lesson, RelevantLesson))]
getPicked = do
    (AnalysisStatic{analysisAllLessons}, AnalysisDynamic{analysisPicked}) <- readIORef globalAnalysisState

    let pairLesson :: V3Id -> RelevantLesson -> (Lesson, RelevantLesson)
        pairLesson lId relevant = (analysisAllLessons Map.! lId, relevant)

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
             $ map show
             $ sortBy (comparing lessonKey)
             $ available
    putStrLn $ ""

    putStrLn $ "Picked lessons (" ++ show (length picked) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n" $ map show picked
    putStrLn $ ""

    putStrLn $ "Words left (" ++ show (length analysisTodo) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate ","
             $ map show
             $ sortBy (comparing wordKey)
             $ todo
    putStrLn $ ""

-- | Show information about a given lesson
infoLesson :: V3Id -> IO ()
infoLesson lessonId = do
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState

    putStrLn . dumpStr $ analysisAllLessons Map.! lessonId

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

    let inKey, inSup :: [V3Id]
        (inKey, inSup) = analysisInverse Map.! source

    available <- mapM (uncurry summarizeLesson) =<< getInFocusAvailable

    putStrLn "This word is in the key vocab of the following available lessons:"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ filter ((`elem` inKey) . lessonSummaryId)
             $ available
    putStrLn $ ""

    putStrLn "This word is in the sup vocab of the following available lessons:"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ filter ((`elem` inSup) . lessonSummaryId)
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
          guard $ any (\word' -> word `isInfixOf` source word') (key lesson ++ sup lesson)
          let mAlreadyPicked      = lookup v3id analysisPicked
              mAvailable          = Map.lookup v3id analysisAvailable
              (relevant, comment) = case (mAlreadyPicked, mAvailable) of
                (Just alreadyPicked, _) -> (alreadyPicked, "already picked")
                (_, Just available)     -> (available, "available")
                (_, _)                  -> (irrelevant lesson, "other")
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
             $ map (\(summary, comment) -> show summary ++ " (" ++ comment ++ ")")
             $ sortBy (comparing (lessonNumKey . fst))
             $ summarized
  where
    irrelevant :: Lesson -> RelevantLesson
    irrelevant Lesson{..} = RelevantLesson{
        relKey = []
      , relSup = []
      , irrelKey = map withLevel key
      , irrelSup = map withLevel sup
      }

{-------------------------------------------------------------------------------
  Different kinds of sorting functions
-------------------------------------------------------------------------------}

-- | Number of relevant words in the key vocabulary of the lesson
lessonNumKey :: LessonSummary -> Int
lessonNumKey LessonSummary{..} = length lessonSummaryRelKey

-- | Number of lessons that have this word in their key vocabulary
wordNumKey :: WordSummary -> Int
wordNumKey WordSummary{..} = length wordSummaryInKey

-- | Number of words in the irrelevant key vocabulary that are not in the
-- specified HSK levels
reallyIrrelevantKey :: [HSKLevel] -> LessonSummary -> Int
reallyIrrelevantKey ls LessonSummary{..} =
  length $ filter (none (`elem` ls) . snd) lessonSummaryIrrelKey

-- | Summarize using default sorting
summarize :: IO ()
summarize = summarizeUsing lessonNumKey wordNumKey

{-------------------------------------------------------------------------------
  Predicates on lessons
-------------------------------------------------------------------------------}

type LessonPredicate = (Lesson, RelevantLesson) -> Bool

atLevel :: [Level] -> LessonPredicate
atLevel ls (Lesson{..}, _) = level `elem` ls

-- | Only allow irrelevant words in the key vocab from the specified HSK levels
keyInHSK :: [HSKLevel] -> LessonPredicate
keyInHSK ls (_, RelevantLesson{..}) = all (any (`elem` ls) . snd) irrelKey

-- | Only allow irrelevant words in the key vocab from the specified HSK levels
supInHSK :: [HSKLevel] -> LessonPredicate
supInHSK ls (_, RelevantLesson{..}) = all (any (`elem` ls) . snd) irrelSup

-- | Maximum number of irrelevant words in the key vocab
maxNumKeyIrrel :: Int -> LessonPredicate
maxNumKeyIrrel n (_, RelevantLesson{..}) = length irrelKey <= n

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
        p' lessonId relevant = p (analysisAllLessons Map.! lessonId, relevant)

-- | Pick a lesson
pick :: V3Id
     -> Bool  -- ^ Should the sup vocab of the lesson be removed from TODO?
     -> IO ()
pick lessonId includeSup = updateAnalysisState aux
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
        pickedLesson = analysisAvailable Map.! lessonId

        pickedLessonWords :: Set Simpl
        pickedLessonWords = simplSet $
          if includeSup then relKey pickedLesson ++ relSup pickedLesson
                        else relKey pickedLesson

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
harmless :: Simpl -> IO ()
harmless w = modifyIORef globalHarmless $ Set.insert w

globalHarmless :: IORef (Set Simpl)
{-# NOINLINE globalHarmless #-}
globalHarmless = unsafePerformIO $ newIORef Set.empty

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
