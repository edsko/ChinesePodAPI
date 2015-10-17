-- | Definition of a Word
--
-- This lives separate from the main Vocab module so that changes to the main
-- "Vocab" module do not trigger a recompile of the HSK module. (This is also
-- why 'extractWord' does not live here, as that would bring in a dependency on
-- the "API" module).
module Servant.ChinesePod.Vocab.Word (Word(..)) where

import Prelude hiding (Word)
import Data.Binary (Binary)
import Data.Data (Data)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..))
import qualified Text.Show.Pretty as P

data Word = Word {
      pinyin :: String
    , source :: String
    , target :: String
    }
  deriving (Generic, Data, Eq, Ord, Show)

instance PrettyVal Word where
  prettyVal Word{..} = P.Con "Word" [
      prettyVal pinyin
    , prettyVal source
    , prettyVal target
    ]

instance Binary Word
