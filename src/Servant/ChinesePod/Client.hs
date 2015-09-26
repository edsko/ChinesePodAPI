module Servant.ChinesePod.Client (
    ChinesePodAPI(..)
  , chinesePodAPI
    -- * Re-exports
  , EitherT(..)
  , ServantError
  , BaseUrl(..)
  , Scheme(..)
  , liftIO
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(..))
import Servant.Client

import Servant.ChinesePod.API

data ChinesePodAPI = ChinesePodAPI {
    cpodLogin :: Client Login
  }

chinesePodAPI :: BaseUrl -> ChinesePodAPI
chinesePodAPI baseUrl =
    let ( cpodLogin ) = client api baseUrl
    in ChinesePodAPI{..}
