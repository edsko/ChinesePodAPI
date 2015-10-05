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
import Data.List (intercalate, sortBy, partition)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (catMaybes)
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
    analysisAvailable = cullRelevant analysisTodo $
                          allRelevant analysisAllLessons

-- | Cull relevant lessons to a new word list
cullRelevant :: [Word]
             -> Map V3Id RelevantLesson
             -> Map V3Id RelevantLesson
cullRelevant words = Map.mapMaybe relevantLesson
  where
    relevantLesson :: RelevantLesson -> Maybe RelevantLesson
    relevantLesson RelevantLesson{..} = do
      let (relKeyKeep, relKeyDrop) = partition isRelevantWord relKey
          (relSupKeep, relSupDrop) = partition isRelevantWord relSup
      guard $ not (null (relKeyKeep ++ relSupKeep))
      return RelevantLesson {
          relKey   = relKeyKeep
        , relSup   = relSupKeep
        , irrelKey = irrelKey ++ map withLevel relKeyDrop
        , irrelSup = irrelSup ++ map withLevel relSupDrop
        }

    isRelevantWord :: Word -> Bool
    isRelevantWord Word{..} = source `Set.member` words'

    words' :: Set Simpl
    words' = Set.fromList $ map source words

    withLevel :: Word -> (Word, [HSKLevel])
    withLevel w = (w, map fst . hskLevel . source $ w)

-- | Initial (crude) set of relevant lessons
--
-- In order to construct a 'relevant' set, we first compute a set where
-- we consider _all_ words to be relevant.
allRelevant :: Map V3Id Lesson -> Map V3Id RelevantLesson
allRelevant = Map.map aux
  where
    aux :: Lesson -> RelevantLesson
    aux Lesson{..} = RelevantLesson{
        relKey   = key
      , relSup   = sup
      , irrelKey = []
      , irrelSup = []
      }

computeInverse :: Map V3Id Lesson
               -> [Simpl]
               -> Map Simpl ([V3Id], [V3Id])
computeInverse vocab = Map.fromList . map go
  where
    vocab' :: [(V3Id, Lesson)]
    vocab' = Map.toList vocab

    go :: Simpl -> (Simpl, ([V3Id], [V3Id]))
    go word = ( word
              , ( catMaybes $ map (containsIn key word) vocab'
                , catMaybes $ map (containsIn sup word) vocab'
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
--
-- For the irrelevant vocabulary we record the HSK levels that word appears in
-- so that we can judge just how irrelevant they are.
data LessonSummary = LessonSummary {
      lessonSummaryId       :: V3Id
    , lessonSummaryTitle    :: String
    , lessonSummaryRelKey   :: [Word]
    , lessonSummaryRelSup   :: [Word]
    , lessonSummaryLevel    :: Level
    , lessonSummaryIrrelKey :: [(Word, [HSKLevel])]
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
      , ": "
      , show $ length wordSummaryInKey
      , "/"
      , show $ length wordSummaryInSup
      ]

-- | Show a readable summary of what's left to do
summarizeUsing :: (Ord a, Ord b) =>
                  (LessonSummary -> a) -- Sort key for lessons
               -> (WordSummary   -> b) -- Sort key forwords
               -> IO ()
summarizeUsing lessonKey wordKey = do
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState

    let pairLesson :: V3Id -> RelevantLesson -> (Lesson, RelevantLesson)
        pairLesson lId relevant = (analysisAllLessons Map.! lId, relevant)

    let summarizeLesson :: V3Id -> (Lesson, RelevantLesson) -> LessonSummary
        summarizeLesson lessonId (Lesson{..}, RelevantLesson{..}) = LessonSummary {
              lessonSummaryId       = lessonId
            , lessonSummaryTitle    = title
            , lessonSummaryRelKey   = relKey
            , lessonSummaryRelSup   = relSup
            , lessonSummaryLevel    = level
            , lessonSummaryIrrelKey = irrelKey
            , lessonSummaryIrrelSup = irrelSup
            }

    let summarizeWord :: Word -> WordSummary
        summarizeWord Word{..} = WordSummary {
              wordSummarySimpl = source
            , wordSummaryInKey = inKey
            , wordSummaryInSup = inSup
            }
          where
            inKey, inSup :: [V3Id]
            (inKey, inSup) = analysisInverse Map.! source

    putStrLn $ "Words left (" ++ show (length analysisTodo) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ sortBy (comparing wordKey)
             $ map summarizeWord
             $ analysisTodo
    putStrLn $ ""

    -- Get available lesson filtered by current focus
    zoomedIn <- isZoomedIn
    inFocus  <- getFocus

    let analysisAvailable', analysisPicked' :: [(V3Id, (Lesson, RelevantLesson))]
        analysisAvailable' = Map.toList
                           $ Map.filter inFocus
                           $ Map.mapWithKey pairLesson
                           $ analysisAvailable

        analysisPicked' = mapWithKey pairLesson
                        $ analysisPicked

    putStrLn $ "Available lessons (" ++ show (length analysisAvailable') ++ ")"
            ++ if zoomedIn then " (zoomed in)" else ""
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ sortBy (comparing lessonKey)
             $ map (uncurry summarizeLesson)
             $ analysisAvailable'
    putStrLn $ ""

    putStrLn $ "Picked lessons (" ++ show (length analysisPicked') ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ map (uncurry summarizeLesson)
             $ analysisPicked'
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

{-------------------------------------------------------------------------------
  Different kinds of sorting functions
-------------------------------------------------------------------------------}

-- | Number of relevant words in the key vocabulary of the lesson
lessonNumKey :: LessonSummary -> Int
lessonNumKey LessonSummary{..} = length lessonSummaryRelKey

-- | Number of lessons that have this word in their key vocabulary
wordNumKey :: WordSummary -> Int
wordNumKey WordSummary{..} = length wordSummaryInKey

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

-- | Pick a class
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
        analysisAvailable' = cullRelevant analysisTodo' $
                               Map.delete lessonId analysisAvailable

        inLesson :: Word -> Bool
        inLesson Word{..} = source `Set.member` pickedLessonWords

        pickedLesson :: RelevantLesson
        pickedLesson = analysisAvailable Map.! lessonId

        -- TODO: This only removes the word if it was in the _key_ vocabulary
        -- for the lesson we picked. We may also want to consider the
        -- supplemental vocabulary.
        pickedLessonWords :: Set Simpl
        pickedLessonWords = Set.fromList $ map source (relKey pickedLesson)

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

showHskLevel :: Simpl -> IO ()
showHskLevel = putStrLn . dumpStr . hskLevel

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mapWithKey :: forall k a b. (k -> a -> b) -> [(k,a)] -> [(k,b)]
mapWithKey f = map go
  where
    go :: (k,a) -> (k,b)
    go (k,a) = (k, f k a)
