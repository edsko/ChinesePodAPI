-- | Analyze the vocab lists and try to match them against dialogues
--
-- This module is intended for use in @ghci@.
module Servant.ChinesePod.Analysis (
    initState
  , globalAnalysisState
  , getStats
    -- Re-exports
  , module Servant.ChinesePod.Vocab
  , module Servant.ChinesePod.HSK.HSK2012
  , module Data.IORef
  ) where

import Prelude hiding (Word)
import Data.IORef
import Data.Map (Map)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (PrettyVal, dumpStr)
import qualified Data.Map as Map

import Servant.ChinesePod.Vocab
import Servant.ChinesePod.HSK.HSK2012

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data AnalysisState = AnalysisState {
      analysisAvailable :: Map V3Id Lesson
    , analysisPicked    :: [V3Id]
    , analysisTodo      :: [Word]
    }
  deriving (Generic)

instance PrettyVal AnalysisState
instance Show AnalysisState where show = dumpStr

globalAnalysisState :: IORef AnalysisState
{-# NOINLINE globalAnalysisState #-}
globalAnalysisState = unsafePerformIO $ newIORef undefined

initState :: FilePath -> [Word] -> IO ()
initState vocabFile todo = do
    Vocab vocab <- loadVocab vocabFile
    writeIORef globalAnalysisState AnalysisState {
        analysisAvailable = vocab
      , analysisPicked    = []
      , analysisTodo      = todo
      }

{-------------------------------------------------------------------------------
  Analysis statistics
-------------------------------------------------------------------------------}

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
