module Servant.ChinesePod.Analysis.State.V1 (
    -- * Types
    Simpl
  , AnalysisStatic(..)
  , AnalysisDynamic(..)
  , AnalysisState
  , RelevantLesson(..)
  , HSKLevel
  ) where

import Prelude hiding (Word, words)
import Data.Binary (Binary)
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import Servant.ChinesePod.Vocab.V1

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

-- | HSK level (1..6)
type HSKLevel = Int

instance PrettyVal AnalysisStatic
instance PrettyVal AnalysisDynamic
instance PrettyVal RelevantLesson

instance Binary AnalysisStatic
instance Binary AnalysisDynamic
instance Binary RelevantLesson
