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
  deriving (Generic)

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
  deriving (Generic)

type AnalysisState = (AnalysisStatic, AnalysisDynamic)

-- | Lesson relevant to the set of words we are studying
--
-- A lesson is "relevant" if
--
-- > not (null (relKey ++ relSup))
data RelevantLesson = RelevantLesson {
      -- | Relevant key vocabulary
      relKey :: [Simpl]

      -- | Relevant supplemental vocabulary
    , relSup :: [Simpl]

      -- | Number of irrelevant words in the key vocabulary
    , irrelKey :: Int

      -- | Number of irrelevant words in the supplemental vocabulary
    , irrelSup :: Int
    }
  deriving (Generic)

instance PrettyVal AnalysisStatic
instance PrettyVal AnalysisDynamic
instance PrettyVal RelevantLesson

instance Binary AnalysisStatic
instance Binary AnalysisDynamic
instance Binary RelevantLesson

instance Show AnalysisStatic  where show = dumpStr
instance Show AnalysisDynamic where show = dumpStr
instance Show RelevantLesson  where show = dumpStr

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
        , irrelKey = irrelKey + length relKeyDrop
        , irrelSup = irrelSup + length relSupDrop
        }

    isRelevantWord :: Simpl -> Bool
    isRelevantWord = (`Set.member` Set.fromList (map source words))

-- | Initial (crude) set of relevant lessons
--
-- In order to construct a 'relevant' set, we first compute a set where
-- we consider _all_ words to be relevant.
allRelevant :: Map V3Id Lesson -> Map V3Id RelevantLesson
allRelevant = Map.map aux
  where
    aux :: Lesson -> RelevantLesson
    aux Lesson{..} = RelevantLesson{
        relKey   = map source key
      , relSup   = map source sup
      , irrelKey = 0
      , irrelSup = 0
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

data LessonSummary = LessonSummary {
      lessonSummaryId       :: V3Id
    , lessonSummaryTitle    :: String
    , lessonSummaryRelKey   :: [Simpl]
    , lessonSummaryRelSup   :: [Simpl]
    , lessonSummaryLevel    :: Level
    , lessonSummaryIrrelKey :: Int
    , lessonSummaryIrrelSup :: Int
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
      , "): "
      , intercalate "," lessonSummaryRelKey
      , "/"
      , intercalate "," lessonSummaryRelSup
      , " ("
      , show $ lessonSummaryLevel
      , ","
      , show $ length lessonSummaryRelKey
      , "/"
      , show $ length lessonSummaryRelSup
      , ","
      , show lessonSummaryIrrelKey
      , "/"
      , show lessonSummaryIrrelSup
      , ")"
      ]

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

    let summarizeLesson :: V3Id -> RelevantLesson -> LessonSummary
        summarizeLesson lessonId RelevantLesson{..} = LessonSummary {
              lessonSummaryId       = lessonId
            , lessonSummaryTitle    = title
            , lessonSummaryRelKey   = relKey
            , lessonSummaryRelSup   = relSup
            , lessonSummaryLevel    = level
            , lessonSummaryIrrelKey = irrelKey
            , lessonSummaryIrrelSup = irrelSup
            }
          where
            Lesson{..} = analysisAllLessons Map.! lessonId

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

    putStrLn $ "Available lessons (" ++ show (length analysisAvailable) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ sortBy (comparing lessonKey)
             $ map (uncurry summarizeLesson)
             $ Map.toList
             $ analysisAvailable
    putStrLn $ ""

    putStrLn $ "Picked lessons (" ++ show (length analysisPicked) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ map (uncurry summarizeLesson)
             $ analysisPicked
    putStrLn $ ""

-- | Show information about a given lesson
infoLesson :: V3Id -> IO ()
infoLesson lessonId = do
    (AnalysisStatic{..}, AnalysisDynamic{..}) <- readIORef globalAnalysisState

    print $ analysisAllLessons Map.! lessonId

    forM_ (lookup lessonId analysisPicked) $ \relevantLesson -> do
      putStrLn "We picked this lesson:"
      print relevantLesson

    forM_ (Map.lookup lessonId analysisAvailable) $ \relevantLesson -> do
      putStrLn "This lesson is available:"
      print relevantLesson

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
  Operations on the dynamic state
-------------------------------------------------------------------------------}

filterLessons :: ((Lesson, RelevantLesson) -> Bool) -> IO ()
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

filterByLevel :: (Level -> Bool) -> IO ()
filterByLevel p = filterLessons (p . level . fst)

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

        -- TODO: This only removes the word if it was in the _key_ vocabulary for
        -- the lesson we picked. We may also want to consider the supplemental
        -- vocabulary.
        inLesson :: Word -> Bool
        inLesson Word{..} = source `elem` relKey pickedLesson

        pickedLesson :: RelevantLesson
        pickedLesson = analysisAvailable Map.! lessonId

{-------------------------------------------------------------------------------
  HSK level for each word
-------------------------------------------------------------------------------}

data HSKLevel a = HSK1 a | HSK2 a | HSK3 a | HSK4 a | HSK5 a | HSK6 a
  deriving (Generic)

instance PrettyVal a => PrettyVal (HSKLevel a)
instance PrettyVal a => Show      (HSKLevel a) where show = dumpStr

-- There is some problem with unicode; hskLevel "爱" doesn't return anything
-- (conversely, this is listed as a word without any corresponding CP lessons)
hskLevel :: Simpl -> [HSKLevel Word]
hskLevel simpl = Map.findWithDefault [] simpl hskIndex

hskIndex :: Map Simpl [HSKLevel Word]
hskIndex = Map.unionsWith (++) [
      indexFor HSK1 hsk1
    , indexFor HSK2 hsk2
    , indexFor HSK3 hsk3
    , indexFor HSK4 hsk4
    , indexFor HSK5 hsk5
    , indexFor HSK6 hsk6
    ]
  where
    indexFor :: (Word -> HSKLevel Word) -> [Word] -> Map Simpl [HSKLevel Word]
    indexFor level = Map.fromList . map go
      where
        go :: Word -> (Simpl, [HSKLevel Word])
        go word@Word{..} = (source, [level word])
