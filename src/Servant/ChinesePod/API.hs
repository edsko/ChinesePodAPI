{-# LANGUAGE OverloadedStrings #-}
module Servant.ChinesePod.API (
    api
    -- * API specification
  , ChinesePod
  , Services
  , Account
  , Login
  , Logout
    -- * Request types
  , ReqLogin(..)
  , ReqSignature(..)
    -- * Response types
  , RespLogin(..)
    -- * Constructing requests from previous responses
  , FromLogin(..)
    -- * ChinesePod specific datatypes
  , AccessToken(..)
  , UserId(..)
  ) where

import Control.Monad
import Crypto.Hash
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Network.URI
import Servant.API
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.Text            as T

api :: Proxy ChinesePod
api = Proxy

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type ChinesePod = "api" :> "0.6" :> Services

type Services = "account" :> Account

type Account = "login"  :> Login
          :<|> "logout" :> Logout

type Login  = ReqBody '[FormUrlEncoded] ReqLogin  :> Post '[JSON] RespLogin
type Logout = ReqBody '[FormUrlEncoded] ReqLogout :> Post '[JSON] OK

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

data ReqLogout = ReqLogout {
      reqLogoutAccessToken :: AccessToken
    , reqLogoutUserId      :: UserId
    }

-- | The 'ToText' instance for 'ReqSignature' is the hash
instance ToText ReqSignature where
    toText ReqSignature{..} =
        toText . show . sha1 . BS.UTF8.fromString $ concat [
            reqSignatureClientSecret
          , reqSignatureUserPassword
          ]
      where
        sha1 :: BS.ByteString -> Digest SHA1
        sha1 = hash

instance ToFormUrlEncoded ReqLogin where
    toFormUrlEncoded ReqLogin{..} = [
          ( "client_id" , toText reqLoginClientId  )
        , ( "email"     , toText reqLoginEmail     )
        , ( "signature" , toText reqLoginSignature )
        ]

instance ToFormUrlEncoded ReqLogout where
    toFormUrlEncoded ReqLogout{..} = [
          ( "access_token" , toText reqLogoutAccessToken )
        , ( "user_id"      , toText reqLogoutUserId      )
        ]

{-------------------------------------------------------------------------------
  Response types
-------------------------------------------------------------------------------}

data RespLogin = RespLogin {
      respLoginAccessToken            :: AccessToken
    , respLoginUserId                 :: UserId
    , respLoginUsername               :: String
    , respLoginName                   :: String
    , respLoginSelfStudyLessonsTotal  :: Int
    , respLoginAssignedLessonsTotal   :: Int
    , respLoginCoursesCount           :: Int
    , respLoginLang                   :: String
    , respLoginBio                    :: String
    , respLoginAvatarUrl              :: Maybe URI
    , respLoginNewLessonNotification  :: Bool
    , respLoginNewShowNotification    :: Bool
    , respLoginNewsletterNotification :: Bool
    , respLoginGeneralNotification    :: Bool
    , respLoginBookmarkedLessons      :: Int
    , respLoginSubscribedLessons      :: Int
    , respLoginStudiedLessons         :: Int
    }
  deriving (Show)

instance FromJSON RespLogin where
    parseJSON = withObject "RespLogin" $ \obj -> do
      respLoginAccessToken                    <- obj .: "access_token"
      respLoginUserId                         <- obj .: "user_id"
      respLoginUsername                       <- obj .: "username"
      respLoginName                           <- obj .: "name"
      Stringy respLoginSelfStudyLessonsTotal  <- obj .: "self_study_lessons_total"
      respLoginAssignedLessonsTotal           <- obj .: "assigned_lessons_total"
      Stringy respLoginCoursesCount           <- obj .: "courses_count"
      respLoginLang                           <- obj .: "lang"
      respLoginBio                            <- obj .: "bio"
      Stringy respLoginAvatarUrl              <- obj .: "avatar_url"
      Stringy respLoginNewLessonNotification  <- obj .: "new_lesson_notification"
      Stringy respLoginNewShowNotification    <- obj .: "new_show_notification"
      Stringy respLoginNewsletterNotification <- obj .: "newsletter_notification"
      Stringy respLoginGeneralNotification    <- obj .: "general_notification"
      respLoginBookmarkedLessons              <- obj .: "bookmarked_lessons"
      respLoginSubscribedLessons              <- obj .: "subscribed_lessons"
      respLoginStudiedLessons                 <- obj .: "studied_lessons"
      return RespLogin{..}

{-------------------------------------------------------------------------------
  ChinesePod specific datatypes
-------------------------------------------------------------------------------}

newtype UserId = UserId String
  deriving (Show, FromJSON, ToText)

newtype AccessToken = AccessToken String
  deriving (Show, FromJSON, ToText)

-- | Some ChinesePod requests simply return OK
data OK = OK
  deriving (Show)

instance FromJSON OK where
    parseJSON = withObject "OK" $ \obj -> do
      result <- obj .: "result"
      case result :: String of
        "OK" -> return OK
        _    -> fail $ "Expected OK"

{-------------------------------------------------------------------------------
  Many requests need information that got returned in the initial login
-------------------------------------------------------------------------------}

class FromLogin a where
    fromLogin :: RespLogin -> a

instance FromLogin ReqLogout where
    fromLogin RespLogin{..} = ReqLogout {
        reqLogoutAccessToken = respLoginAccessToken
      , reqLogoutUserId      = respLoginUserId
      }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Value encoded in JSON as a string
--
-- The ChinesePOD API encodes a number of values as strings; i.e., it might
-- represent @False@ as @"0"@ rather than a JSON bool value.
newtype Stringy a = Stringy a

instance FromJSON (Stringy Int) where
    parseJSON = liftM Stringy . withText "Int" tryRead

instance FromJSON (Stringy (Maybe URI)) where
    parseJSON = withText "URI" $ return . Stringy . parseURI . T.unpack

instance FromJSON (Stringy Bool) where
    parseJSON = withText "Bool" $ \txt ->
      case txt of
        "0" -> return $ Stringy False
        "1" -> return $ Stringy True
        _   -> fail $ "Could not parse bool " ++ show txt

tryRead :: forall a. (Typeable a, Read a) => Text -> Parser a
tryRead strA =
     case filter fullParse (readsPrec 0 (T.unpack strA)) of
       [(a, _)]   -> return a
       _otherwise -> fail $ "Could not parse " ++ show strA ++ " "
                         ++ "as " ++ show (typeOf (undefined :: a))
   where
      fullParse :: (a, String) -> Bool
      fullParse = null . snd
