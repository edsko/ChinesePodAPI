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
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (catMaybes)
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

{-------------------------------------------------------------------------------
  Query the state
-------------------------------------------------------------------------------}

-- | Show a readable summary of what's left to do
summarize :: IO ()
summarize = do
    AnalysisState{..} <- readIORef globalAnalysisState

    let summarizeLesson :: RelevantLesson -> String
        summarizeLesson RelevantLesson{..} = concat [
            v3IdString relId
          , " ("
          , title (analysisAllLessons Map.! relId)
          , "): "
          , intercalate "," relKey
          , ";"
          , intercalate "," relSup
          , " ("
          , show (level (analysisAllLessons Map.! relId))
          , ","
          , show (length relKey)
          , ";"
          , show (length relSup)
          , ","
          , show irrelKey
          , ";"
          , show irrelSup
          , ")"
          ]

    putStrLn $ "Words left (" ++ show (Set.size analysisTodo) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "," $ Set.toList analysisTodo
    putStrLn $ ""

    putStrLn $ "Available lessons (" ++ show (length analysisAvailable) ++ ")"
    putStrLn $ "---------------------------------------------------------------"
    putStrLn $ intercalate "\n" $ map summarizeLesson analysisAvailable
    putStrLn $ ""



{-------------------------------------------------------------------------------
  Analysis statistics
-------------------------------------------------------------------------------}

{-
data Stats = Stats {
      statsAvailable :: Int
    , statsPicked    :: Int
    , statsTodo      :: Int
    }
  deriving (Generic)

instance PrettyVal Stats
instance Show Stats where show = dumpStr

stats :: AnalysisState -> Stats
stats AnalysisState{..} = Stats {
      statsAvailable = Map.size analysisAvailable
    , statsPicked    = length analysisPicked
    , statsTodo      = length analysisTodo
    }

getStats :: IO Stats
getStats = stats <$> readIORef globalAnalysisState
-}
