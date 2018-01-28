module Servant.ChinesePod.Util.Migrate (
    Migrate(..)
  ) where

-- | Adhoc migration between types
--
-- This isn't very elegant, because when type A refers to B, refers to C,
-- ..., and type Z gets changed, we need migration policies for all of A..Z.
-- Oh well, so be it.
class Migrate a where
  type MigrateFrom a :: *

  migrate :: MigrateFrom a -> a
