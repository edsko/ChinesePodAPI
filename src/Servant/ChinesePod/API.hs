{-# LANGUAGE OverloadedStrings #-}
module Servant.ChinesePod.API (
    api
    -- * API specification
  , ChinesePod
  , Services
  , Account
  , Library
    -- ** Account
  , Login
  , Logout
  , GetUserInfo
    -- ** Library
  , SearchLessons
    -- * Request types
  , ReqLogin(..)
  , ReqSignature(..)
  , ReqSearchLessons(..)
    -- * Response types
  , RespLogin(..)
  , RespSearchLessons
    -- * Constructing requests from previous responses
  , FromLogin(..)
    -- * Auxiliary dataypes used to define the API
  , OK(..)
  , Undocumented
  , SearchResults(..)
    -- * ChinesePod specific datatypes
  , AccessToken(..)
  , UserId(..)
  , Level(..)
  , Lesson(..)
  ) where

import Control.Monad
import Crypto.Hash
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Network.URI
import Servant.API
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text            as T

api :: Proxy ChinesePod
api = Proxy

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type ChinesePod = "api" :> "0.6" :> Services

type Services = "account" :> Account
           :<|> "library" :> Library

type Account = "login"         :> Login
          :<|> "logout"        :> Logout
          :<|> "get-user-info" :> GetUserInfo

type Login       = ReqBody '[FormUrlEncoded] ReqLogin       :> Post '[JSON] RespLogin
type Logout      = ReqBody '[FormUrlEncoded] ReqLogout      :> Post '[JSON] OK
type GetUserInfo = ReqBody '[FormUrlEncoded] ReqGetUserInfo :> Post '[JSON] RespGetUserInfo

type Library = "search-lessons" :> SearchLessons

type SearchLessons = ReqBody '[FormUrlEncoded] ReqSearchLessons :> Post '[JSON] RespSearchLessons

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
  deriving (Show)

data ReqGetUserInfo = ReqGetUserInfo {
      reqGetUserInfoAccessToken :: AccessToken
    , reqGetUserInfoUserId      :: UserId
    }
  deriving (Show)

data ReqSearchLessons = ReqSearchLessons {
      reqSearchLessonsAccessToken :: AccessToken
    , reqSearchLessonsUserId      :: UserId
    , reqSearchLessonsSearch      :: String
    , reqSearchLessonsSearchLevel :: Maybe Level
    , reqSearchLessonsNumResults  :: Maybe Int
    , reqSearchLessonsPage        :: Maybe Int
    }
  deriving (Show)

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

instance ToFormUrlEncoded ReqGetUserInfo where
    toFormUrlEncoded ReqGetUserInfo{..} = [
          ( "access_token" , toText reqGetUserInfoAccessToken )
        , ( "user_id"      , toText reqGetUserInfoUserId      )
        ]

instance ToFormUrlEncoded ReqSearchLessons where
    toFormUrlEncoded ReqSearchLessons{..} = [
          ( "access_token" , toText reqSearchLessonsAccessToken )
        , ( "user_id"      , toText reqSearchLessonsUserId      )
        , ( "search"       , toText reqSearchLessonsSearch      )
        ] ++ catMaybes [
          optFormArg "search_level" (toOptText . Stringy) reqSearchLessonsSearchLevel
        , optFormArg "num_results"  (Just . toText)       reqSearchLessonsNumResults
        , optFormArg "page"         (Just . toText)       reqSearchLessonsPage
        ]

optFormArg :: Text -> (a -> Maybe Text) -> Maybe a -> Maybe (Text, Text)
optFormArg nm f ma = do
   a   <- ma
   val <- f a
   return (nm, val)

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

data RespGetUserInfo = RespGetUserInfo {
      respGetUserInfoName                     :: String
    , respGetUserInfoUsername                 :: String
    , respGetUserInfoAvatarUrl                :: Maybe URI
    , respGetUserInfoBio                      :: String
    , respGetUserInfoUseTraditionalCharacters :: Bool
    , respGetUserInfoUserId                   :: UserId
    , respGetUserInfoNewLessonNotification    :: Bool
    , respGetUserInfoNewShowNotification      :: Bool
    , respGetUserInfoNewsletterNotification   :: Bool
    , respGetUserInfoGeneralNotification      :: Bool
    , respGetUserInfoLevel                    :: Level
    , respGetUserInfoType                     :: Undocumented String
    }
  deriving (Show)

type RespSearchLessons = SearchResults Lesson

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

instance FromJSON RespGetUserInfo where
    parseJSON = withObject "RespGetUserInfo" $ \obj -> do
      respGetUserInfoName                           <- obj .:  "name"
      respGetUserInfoUsername                       <- obj .:  "username"
      Stringy respGetUserInfoAvatarUrl              <- obj .:  "avatar_url"
      respGetUserInfoBio                            <- obj .:  "bio"
      Nummy respGetUserInfoUseTraditionalCharacters <- obj .:  "use_traditional_characters"
      Nummy respGetUserInfoUserId                   <- obj .:  "user_id"
      Nummy respGetUserInfoNewLessonNotification    <- obj .:  "new_lesson_notification"
      Nummy respGetUserInfoNewShowNotification      <- obj .:  "new_show_notification"
      Nummy respGetUserInfoNewsletterNotification   <- obj .:  "newsletter_notification"
      Nummy respGetUserInfoGeneralNotification      <- obj .:  "general_notification"
      Nummy respGetUserInfoLevel                    <- obj .:  "level"
      respGetUserInfoType                           <- obj .:? "type"
      return RespGetUserInfo{..}

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

-- | User level
data Level =
    LevelNotSpecified
  | LevelNewbie
  | LevelElementary
  | LevelIntermediate
  | LevelUpperIntermediate
  | LevelAdvanced
  deriving (Show)

-- | Some requests return more info than is documented in the API
--
-- Since we should not rely on these fields being set, we mark them.
type Undocumented = Maybe

data Lesson = Lesson {
      lessonV3Id                 :: String
    , lessonTitle                :: String
    , lessonIntroduction         :: String
    , lessonLevel                :: Level
    , lessonName                 :: String
    , lessonSlug                 :: String
    , lessonLessonId             :: Maybe String
    , lessonPublicationTimestamp :: String
    , lessonImage                :: Maybe URI
    , lessonBookMarked           :: Bool
    , lessonMarkAsStudied        :: Bool
    }
  deriving (Show)

instance FromJSON Lesson where
    parseJSON = withObject "Lesson" $ \obj -> do
      lessonV3Id                 <- obj .:  "v3_id"
      lessonTitle                <- obj .:  "title"
      lessonIntroduction         <- obj .:  "introduction"
      Stringy lessonLevel        <- obj .:  "level"
      lessonName                 <- obj .:  "name"
      lessonSlug                 <- obj .:  "slug"
      Nullable lessonLessonId    <- obj .:? "lesson_id" .!= Nullable Nothing
      lessonPublicationTimestamp <- obj .:  "publication_timestamp"
      Stringy lessonImage        <- obj .:  "image"
      Nummy lessonBookMarked     <- obj .:  "book_marked"
      Nummy lessonMarkAsStudied  <- obj .:  "mark_as_studied"
      return Lesson{..}

{-------------------------------------------------------------------------------
  Many requests need information that got returned in the initial login
-------------------------------------------------------------------------------}

class FromLogin a b where
    fromLogin :: RespLogin -> b -> a

instance FromLogin ReqLogout () where
    fromLogin RespLogin{..} () = ReqLogout {
        reqLogoutAccessToken = respLoginAccessToken
      , reqLogoutUserId      = respLoginUserId
      }

instance FromLogin ReqGetUserInfo () where
    fromLogin RespLogin{..} () = ReqGetUserInfo {
        reqGetUserInfoAccessToken = respLoginAccessToken
      , reqGetUserInfoUserId      = respLoginUserId
      }

{-------------------------------------------------------------------------------
  String-encoded types
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
    parseJSON = withText "Bool" $ \case
      "0" -> return $ Stringy False
      "1" -> return $ Stringy True
      txt -> fail $ "Could not parse bool " ++ show txt

instance ToOptText (Stringy Level) where
    toOptText (Stringy level) = go level
      where
        go :: Level -> Maybe Text
        go LevelNotSpecified      = Nothing
        go LevelNewbie            = Just "Newbie"
        go LevelElementary        = Just "Elementary"
        go LevelIntermediate      = Just "Intermediate"
        go LevelUpperIntermediate = Just "Upper Intermediate"
        go LevelAdvanced          = Just "Advanced"

instance FromText Level where
     fromText = \case
       "Newbie"             -> Just LevelNewbie
       "Elementary"         -> Just LevelElementary
       "Intermediate"       -> Just LevelIntermediate
       "Upper Intermediate" -> Just LevelUpperIntermediate
       "Advanced"           -> Just LevelAdvanced
       _otherwise           -> Nothing

instance FromJSON (Stringy Level) where
     parseJSON = withText "Level" $ \txt ->
       case fromText txt of
         Just level -> return $ Stringy level
         Nothing    -> fail $ "Could not parse level " ++ show txt

{-------------------------------------------------------------------------------
  Number-encoded types
-------------------------------------------------------------------------------}

-- | Value encoded in JSON as a number
--
-- Like 'Stringy', but using numbers instead.
newtype Nummy a = Nummy a

instance FromJSON (Nummy Bool) where
    parseJSON = withInt "Bool" $ \case
      0 -> return $ Nummy False
      1 -> return $ Nummy True
      i -> fail $ "Could not parse bool " ++ show i

instance FromJSON (Nummy UserId) where
    parseJSON = withInt "UserId" $ return . Nummy . UserId . show

instance FromJSON (Nummy Level) where
     parseJSON = withInt "Level" $ \case
       0 -> return $ Nummy LevelNotSpecified
       1 -> return $ Nummy LevelNewbie
       2 -> return $ Nummy LevelElementary
       3 -> return $ Nummy LevelIntermediate
       4 -> return $ Nummy LevelUpperIntermediate
       5 -> return $ Nummy LevelAdvanced
       i -> fail $ "Could not parse user level " ++ show i

{-------------------------------------------------------------------------------
  Nullable fields (fields whose absence is represented with an explicit 'null')
-------------------------------------------------------------------------------}

newtype Nullable a = Nullable (Maybe a)

instance FromJSON a => FromJSON (Nullable a) where
    parseJSON Null = return $ Nullable Nothing
    parseJSON val  = Nullable . Just <$> parseJSON val

{-------------------------------------------------------------------------------
  Some values are represented by their absence
-------------------------------------------------------------------------------}

class ToOptText a where
  toOptText :: a -> Maybe Text

{-------------------------------------------------------------------------------
  Auxiliary aeson
-------------------------------------------------------------------------------}

tryRead :: forall a. (Typeable a, Read a) => Text -> Parser a
tryRead strA =
     case filter fullParse (readsPrec 0 (T.unpack strA)) of
       [(a, _)]   -> return a
       _otherwise -> fail $ "Could not parse " ++ show strA ++ " "
                         ++ "as " ++ show (typeOf (undefined :: a))
   where
      fullParse :: (a, String) -> Bool
      fullParse = null . snd

withInt :: String -> (Int -> Parser a) -> Value -> Parser a
withInt expected f = withScientific expected $ f . round

{-------------------------------------------------------------------------------
  Generic search results
-------------------------------------------------------------------------------}

data SearchResults a = SearchResults {
      searchResults      :: [a]
    , searchResultsTotal :: Int
    }
  deriving (Show)

instance FromJSON a => FromJSON (SearchResults a) where
    parseJSON = withObject "SearchResults" $ \obj -> do
        let rawResults = catMaybes $ map extractRaw (HashMap.toList obj)
        searchResults      <- mapM parseJSON (sortRaw rawResults)
        searchResultsTotal <- obj .: "total"
        return SearchResults{..}
      where
        extractRaw :: (Text, Value) -> Maybe (Int, Value)
        extractRaw (idx, val) = do idx' <- fromText idx ; return (idx', val)

        sortRaw :: [(Int, Value)] -> [Value]
        sortRaw = map snd . sortBy (comparing fst)
