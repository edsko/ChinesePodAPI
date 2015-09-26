{-# LANGUAGE OverloadedStrings #-}
module Servant.ChinesePod.API (
    api
    -- * API specification
  , ChinesePod
  , Services
  , Account
  , Login
    -- * Request types
  , ReqLogin(..)
  , ReqSignature(..)
    -- * Response types
  , RespLogin(..)
  ) where

import Crypto.Hash
import Data.Aeson
import Data.Text (Text)
import Data.Proxy
import Servant.API
import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.Text            as T

api :: Proxy ChinesePod
api = Proxy

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type ChinesePod = "api" :> "0.6" :> Services

type Services = "account" :> Account

type Account = "login" :> Login

type Login = ReqBody '[FormUrlEncoded] ReqLogin :> Post '[JSON] RespLogin

{-------------------------------------------------------------------------------
  Request types
-------------------------------------------------------------------------------}

data ReqLogin = ReqLogin {
      reqLoginClientId  :: String
    , reqLoginEmail     :: String
    , reqLoginSignature :: ReqSignature
    }
  deriving (Show)

data ReqSignature = ReqSignature {
      reqSignatureClientSecret :: String
    , reqSignatureUserPassword :: String
    }
  deriving (Show)

sign :: ReqSignature -> Digest SHA1
sign ReqSignature{..} = hash $ BS.UTF8.fromString $ concat [
      reqSignatureClientSecret
    , reqSignatureUserPassword
    ]

instance ToFormUrlEncoded ReqLogin where
    toFormUrlEncoded ReqLogin{..} = [
          ( "client_id" , T.pack reqLoginClientId )
        , ( "email"     , T.pack reqLoginEmail    )
        , ( "signature" , sign' reqLoginSignature )
        ]
      where
        sign' :: ReqSignature -> Text
        sign' = T.pack . show . sign

{-------------------------------------------------------------------------------
  Response types
-------------------------------------------------------------------------------}

data RespLogin = RespLogin {
      respLoginAccessToken :: String
    }
  deriving (Show)

instance FromJSON RespLogin where
    parseJSON = withObject "RespLogin" $ \obj -> do
      respLoginAccessToken <- obj .: "access_token"
      return RespLogin{..}
