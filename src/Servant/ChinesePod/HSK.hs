module Servant.ChinesePod.HSK (
    module Servant.ChinesePod.HSK.HSK2012
  , HSKLevel
  , Simpl
    -- * Word index (词典)
  , hskAllWords
  , hskWordIndex
  , hskWordLevel
    -- * Character index (字典)
  , hskAllChars
  , hskCharIndex
  , hskCharLevel
  ) where

import Prelude hiding (Word)
import Data.Bifunctor
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Servant.ChinesePod.HSK.HSK2012
import Servant.ChinesePod.Vocab.Word

-- | HSK level (1..6)
type HSKLevel = Int

-- | Word in simplified characters
type Simpl = String

{-------------------------------------------------------------------------------
  Word index (词典)
-------------------------------------------------------------------------------}

hskAllWords :: [(HSKLevel, [Word])]
hskAllWords = [
      (1, hsk1)
    , (2, hsk2)
    , (3, hsk3)
    , (4, hsk4)
    , (5, hsk5)
    , (6, hsk6)
    ]

hskWordIndex :: Map Simpl [(HSKLevel, Word)]
hskWordIndex = Map.unionsWith (++) $ map (uncurry indexFor) hskAllWords
  where
    indexFor :: HSKLevel -> [Word] -> Map Simpl [(HSKLevel, Word)]
    indexFor level = Map.fromList . map go
      where
        go :: Word -> (Simpl, [(HSKLevel, Word)])
        go word@Word{..} = (source, [(level, word)])

hskWordLevel :: Simpl -> [(HSKLevel, Word)]
hskWordLevel simpl = Map.findWithDefault [] simpl hskWordIndex

{-------------------------------------------------------------------------------
  Character index (字典)
-------------------------------------------------------------------------------}

hskAllChars :: [(HSKLevel, Set Char)]
hskAllChars = removeDups Set.empty
            $ map (second (Set.fromList . concatMap source)) hskAllWords
  where
    removeDups :: Ord b => Set b -> [(a, Set b)] -> [(a, Set b)]
    removeDups _   []            = []
    removeDups acc ((a, bs):bss) =
          (a, Set.filter (not . (`Set.member` acc)) bs)
        : removeDups (acc `Set.union` bs) bss

hskCharIndex :: Map Char HSKLevel
hskCharIndex = Map.fromList $ map swap $ flatten $ hskAllChars
  where
    flatten :: [(a, Set b)] -> [(a, b)]
    flatten = concatMap (\(a, bs) -> map (a,) (Set.toList bs))

hskCharLevel :: Char -> Maybe HSKLevel
hskCharLevel = (`Map.lookup` hskCharIndex)
