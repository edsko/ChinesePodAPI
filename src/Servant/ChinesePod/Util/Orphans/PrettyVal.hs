{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.ChinesePod.Util.Orphans.PrettyVal () where

import Data.Map (Map)
import Text.Show.Pretty
import qualified Data.Map as Map

instance PrettyVal Bool

instance PrettyVal a => PrettyVal (Maybe a)

instance (PrettyVal k, PrettyVal v) => PrettyVal (Map k v) where
  prettyVal = prettyVal . Map.toList
