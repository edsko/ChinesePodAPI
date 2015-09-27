{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.ChinesePod.Util.Orphans.PrettyVal () where

import Data.Map (Map)
import Text.Show.Pretty
import qualified Data.Map                  as Map
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Encode.Pretty  as Aeson.Pretty
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8

{-------------------------------------------------------------------------------
  Base
-------------------------------------------------------------------------------}

instance PrettyVal Bool

instance PrettyVal a => PrettyVal (Maybe a)

{-------------------------------------------------------------------------------
  Containers
-------------------------------------------------------------------------------}

instance (PrettyVal k, PrettyVal v) => PrettyVal (Map k v) where
  prettyVal = prettyVal . Map.toList

{-------------------------------------------------------------------------------
  Aeson
-------------------------------------------------------------------------------}

instance PrettyVal Aeson.Value where
  prettyVal = String . BS.L.UTF8.toString . Aeson.Pretty.encodePretty
