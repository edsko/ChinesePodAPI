module Servant.ChinesePod.Client (
    ChinesePodAPI(..)
  , chinesePodAPI
    -- * Re-exports
  , ServantError
  , BaseUrl(..)
  , Scheme(..)
  , liftIO
  ) where

import Control.Monad.IO.Class (liftIO)
import Servant.API
import Servant.Client

import Servant.ChinesePod.API

data ChinesePodAPI = ChinesePodAPI {
    cpodLogin            :: Client ClientM Login
  , cpodLogout           :: Client ClientM Logout
  , cpodGetUserInfo      :: Client ClientM GetUserInfo
  , cpodGetLesson        :: Client ClientM GetLesson
  , cpodGetLatestLessons :: Client ClientM GetLatestLessons
  , cpodSearchLessons    :: Client ClientM SearchLessons
  }

chinesePodAPI :: ChinesePodAPI
chinesePodAPI = ChinesePodAPI{..}
  where
    (     cpodLogin
     :<|> cpodLogout
     :<|> cpodGetUserInfo
     :<|> cpodGetLesson
     :<|> cpodGetLatestLessons
     :<|> cpodSearchLessons
     ) = client api
