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
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
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

      -- | The "relevant" lessons to this set of words
    , analysisRelevant :: Map V3Id RelevantLesson
    }
  deriving (Generic)

-- | The dynamic part of the analysis: words we've covered, lesosns we picked
data AnalysisDynamic = AnalysisDynamic {
      -- | Words we have left to chose lessons for
      analysisTodo :: [Word]

      -- | Lessons we already chosen (in the order we picked them)
    , analysisPicked :: [V3Id]

      -- | Lessons still available
      --
      -- There may be lessons not in 'analysisAvailable' and not in
      -- 'analysisPicked', if we remove them for other reasons.
    , analysisAvailable :: [V3Id]
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
    , analysisRelevant   = findRelevant vocab
    }
  where
    findRelevant :: Map V3Id Lesson -> Map V3Id RelevantLesson
    findRelevant = Map.fromList
                 . catMaybes
                 . map (relevantLesson (Set.fromList words'))
                 . Map.toList

    words' :: [Simpl]
    words' = map source words

-- | Initial dynamic analysis state
analysisDynamic :: AnalysisStatic -> AnalysisDynamic
analysisDynamic AnalysisStatic{..} = AnalysisDynamic{
      analysisTodo      = analysisAllWords
    , analysisPicked    = []
    , analysisAvailable = Map.keys analysisRelevant
    }

-- | Check if a lesson is relevant to the set of words we are studying
--
-- If relevant, return initial statistics for the lesosn.
relevantLesson :: Set String
               -> (V3Id, Lesson)
               -> Maybe (V3Id, RelevantLesson)
relevantLesson words (lessonId, Lesson{..}) = do
    guard $ not (null (relKey ++ relSup))
    return (lessonId, RelevantLesson{..})
  where
    relKey   = catMaybes (map relevantWord key)
    relSup   = catMaybes (map relevantWord sup)
    irrelKey = length key - length relKey
    irrelSup = length sup - length relSup

    relevantWord :: Word -> Maybe Simpl
    relevantWord Word{..} = do
      guard $ source `Set.member` words
      return source

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

    let summarizeLesson :: V3Id -> LessonSummary
        summarizeLesson lessonId = LessonSummary {
              lessonSummaryId       = lessonId
            , lessonSummaryTitle    = title
            , lessonSummaryRelKey   = relKey
            , lessonSummaryRelSup   = relSup
            , lessonSummaryLevel    = level
            , lessonSummaryIrrelKey = irrelKey
            , lessonSummaryIrrelSup = irrelSup
            }
          where
            RelevantLesson{..} = analysisRelevant   Map.! lessonId
            Lesson{..}         = analysisAllLessons Map.! lessonId

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
             $ map summarizeLesson
             $ analysisAvailable
    putStrLn $ ""

    putStrLn $ "Picked lessons (" ++ show (length analysisPicked) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map (show . summarizeLesson)
             $ analysisPicked
    putStrLn $ ""

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
filterLessons p = modifyIORef globalAnalysisState aux
  where
    -- We only modify the dynamic part but need the static part in order
    -- to provide the predicate with the full info about the lesson
    aux :: (AnalysisStatic, AnalysisDynamic)
        -> (AnalysisStatic, AnalysisDynamic)
    aux (AnalysisStatic{..}, AnalysisDynamic{..}) =
        ( AnalysisStatic{..}
        , AnalysisDynamic{
             analysisAvailable = filter p' analysisAvailable
           , ..
          }
        )
      where
        p' :: V3Id -> Bool
        p' lessonId = p ( analysisAllLessons Map.! lessonId
                        , analysisRelevant   Map.! lessonId
                        )

filterByLevel :: (Level -> Bool) -> IO ()
filterByLevel p = filterLessons (p . level . fst)

-- | Pick a class
pick :: V3Id -> IO ()
pick lessonId = modifyIORef globalAnalysisState aux
  where
    aux :: (AnalysisStatic, AnalysisDynamic)
        -> (AnalysisStatic, AnalysisDynamic)
    aux (AnalysisStatic{..}, AnalysisDynamic{..}) =
        ( AnalysisStatic{..}
        , AnalysisDynamic{
            analysisTodo      = filter (not . inLesson) analysisTodo
          , analysisPicked    = analysisPicked ++ [lessonId]
          , analysisAvailable = filter (/= lessonId) analysisAvailable
          }
        )
      where
        -- TODO: This only removes the word if it was in the _key_ vocabulary for
        -- the lesson we picked. We may also want to consider the supplemental
        -- vocabulary.
        inLesson :: Word -> Bool
        inLesson Word{..} = source `elem` relKey pickedLesson

        pickedLesson :: RelevantLesson
        pickedLesson = analysisRelevant Map.! lessonId
