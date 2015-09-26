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
import Servant.API
import Servant.Client

import Servant.ChinesePod.API

data ChinesePodAPI = ChinesePodAPI {
    cpodLogin            :: Client Login
  , cpodLogout           :: Client Logout
  , cpodGetUserInfo      :: Client GetUserInfo
  , cpodGetLatestLessons :: Client GetLatestLessons
  , cpodSearchLessons    :: Client SearchLessons
  }

chinesePodAPI :: BaseUrl -> ChinesePodAPI
chinesePodAPI baseUrl = ChinesePodAPI{..}
  where
    (     (    cpodLogin
          :<|> cpodLogout
          :<|> cpodGetUserInfo
          )
     :<|> (    cpodGetLatestLessons
          :<|> cpodSearchLessons
          )
     ) = client api baseUrl
