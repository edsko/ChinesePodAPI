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

data AnalysisState = AnalysisState {
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

      -- | Words we have left to chose lessons for
    , analysisTodo :: Set Simpl

      -- | Relevant lessons that are stil available to chose
    , analysisAvailable :: [RelevantLesson]

      -- | Lessons we already chosen
    , analysisPicked :: [RelevantLesson]
    }
  deriving (Generic)

-- | Lesson relevant to the set of words we are studying
--
-- A lesson is "relevant" if
--
-- > not (null (relKey ++ relSup))
data RelevantLesson = RelevantLesson {
      -- | Lesson ID
      relId :: V3Id

      -- | Relevant key vocabulary
    , relKey :: [Simpl]

      -- | Relevant supplemental vocabulary
    , relSup :: [Simpl]

      -- | Number of irrelevant words in the key vocabulary
    , irrelKey :: Int

      -- | Number of irrelevant words in the supplemental vocabulary
    , irrelSup :: Int
    }
  deriving (Generic)

instance PrettyVal AnalysisState
instance PrettyVal RelevantLesson

instance Show AnalysisState  where show = dumpStr
instance Show RelevantLesson where show = dumpStr

globalAnalysisState :: IORef AnalysisState
{-# NOINLINE globalAnalysisState #-}
globalAnalysisState = unsafePerformIO $ newIORef undefined

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initState :: FilePath -> [Word] -> IO ()
initState vocabFile todo = do
    Vocab vocab <- loadVocab vocabFile

    let analysisAllLessons = vocab
        analysisAllWords   = todo
        analysisInverse    = computeInverse vocab analysisTodo
        analysisTodo       = Set.fromList (map source todo)
        analysisAvailable  = findRelevant analysisTodo vocab
        analysisPicked     = []

    writeIORef globalAnalysisState AnalysisState {..}
  where
    findRelevant :: Set Simpl -> Map V3Id Lesson -> [RelevantLesson]
    findRelevant words = catMaybes
                       . map (uncurry (relevantLesson words))
                       . Map.toList

-- | Check if a lesson is relevant to the set of words we are studying
--
-- If relevant, return initial statistics for the lesosn.
relevantLesson :: Set String -> V3Id -> Lesson -> Maybe RelevantLesson
relevantLesson words lessonId Lesson{..} = do
    guard $ not (null (relKey ++ relSup))
    return RelevantLesson{..}
  where
    relId    = lessonId
    relKey   = catMaybes (map relevantWord key)
    relSup   = catMaybes (map relevantWord sup)
    irrelKey = length key - length relKey
    irrelSup = length sup - length relSup

    relevantWord :: Word -> Maybe Simpl
    relevantWord Word{..} = do
      guard $ source `Set.member` words
      return source

computeInverse :: Map V3Id Lesson
               -> Set Simpl
               -> Map Simpl ([V3Id], [V3Id])
computeInverse vocab =
    Map.fromList . map go . Set.toList
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
    AnalysisState{..} <- readIORef globalAnalysisState

    let summarizeLesson :: RelevantLesson -> LessonSummary
        summarizeLesson RelevantLesson{..} = LessonSummary {
              lessonSummaryId       = relId
            , lessonSummaryTitle    = title $ analysisAllLessons Map.! relId
            , lessonSummaryRelKey   = relKey
            , lessonSummaryRelSup   = relSup
            , lessonSummaryLevel    = level $ analysisAllLessons Map.! relId
            , lessonSummaryIrrelKey = irrelKey
            , lessonSummaryIrrelSup = irrelSup
            }

    let summarizeWord :: Simpl -> WordSummary
        summarizeWord word = WordSummary {
              wordSummarySimpl = word
            , wordSummaryInKey = inKey
            , wordSummaryInSup = inSup
            }
          where
            inKey, inSup :: [V3Id]
            (inKey, inSup) = analysisInverse Map.! word

    putStrLn $ "Words left (" ++ show (Set.size analysisTodo) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ sortBy (comparing wordKey)
             $ map summarizeWord
             $ Set.toList analysisTodo
    putStrLn $ ""

    putStrLn $ "Available lessons (" ++ show (length analysisAvailable) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n"
             $ map show
             $ sortBy (comparing lessonKey)
             $ map summarizeLesson
             $ analysisAvailable
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
