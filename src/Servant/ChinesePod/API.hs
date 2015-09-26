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

import Control.Monad
import Crypto.Hash
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Network.URI
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
      respLoginAccessToken            :: String
    , respLoginUserId                 :: String
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
