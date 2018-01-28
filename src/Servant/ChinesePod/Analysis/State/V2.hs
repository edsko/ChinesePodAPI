module Servant.ChinesePod.Analysis.State.V2 (
    -- * Types
    V1.Simpl
  , AnalysisStatic(..)
  , V1.AnalysisDynamic(..)
  , AnalysisState
  , V1.RelevantLesson(..)
  , V1.HSKLevel
    -- * Migration
  , migrate
  ) where

import Prelude hiding (Word, words)
import Data.Binary (Binary)
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import Servant.ChinesePod.Vocab.V2
import Servant.ChinesePod.Util.Migrate

import qualified Servant.ChinesePod.Analysis.State.V1 as V1

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

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
    , analysisInverse :: Map V1.Simpl [V3Id]
    }
  deriving (Generic, Show)

type AnalysisState = (AnalysisStatic, V1.AnalysisDynamic)

instance PrettyVal AnalysisStatic
instance Binary    AnalysisStatic

{-------------------------------------------------------------------------------
  Migration
-------------------------------------------------------------------------------}

instance Migrate AnalysisState where
  type MigrateFrom AnalysisState = V1.AnalysisState

  migrate (static, dynamic) = (migrate static, dynamic)

instance Migrate AnalysisStatic where
  type MigrateFrom AnalysisStatic = V1.AnalysisStatic

  migrate V1.AnalysisStatic{..} = AnalysisStatic{
        analysisAllLessons = fmap migrate analysisAllLessons
      , analysisAllWords   = analysisAllWords
      , analysisInverse    = analysisInverse
      }
