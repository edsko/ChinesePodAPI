{-# LANGUAGE OverloadedStrings #-}
module Servant.ChinesePod.API (
    api
    -- * API specification
  , ChinesePod
    -- ** Account
  , GetUserInfo
  , Login
  , Logout
    -- ** Lesson
  , GetLesson
    -- ** Library
  , GetLatestLessons
  , SearchLessons
    -- * Request types
  , ReqLogin(..)
  , ReqSignature(..)
  , ReqGetLesson(..)
  , ReqSearchLessons(..)
  , ReqGetLatestLessons(..)
    -- * Response types
  , RespLogin(..)
  , RespSearchLessons
    -- * Constructing requests from previous responses
  , FromLogin(..)
    -- * ChinesePod specific datatypes
  , AccessToken(..)
  , UserId(..)
  , Level(..)
  , Lesson(..)
  , LessonContent(..)
  , V3Id(..)
    -- * Auxiliary dataypes used to define the API
  , OK(..)
  , Undocumented
  , SearchResults(..)
  , StrOrInt(..)
    -- * Auxiliary
  , tryRead
  , parseFailure
  ) where

import Crypto.Hash
import Data.Aeson.Types
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Servant.API
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Map             as Map
import qualified Data.Text            as T

api :: Proxy ChinesePod
api = Proxy

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type ChinesePod = "api" :> "0.6" :> Services

type Services = "account" :> "login"              :> Login
           :<|> "account" :> "logout"             :> Logout
           :<|> "account" :> "get-user-info"      :> GetUserInfo
           :<|> "lesson"  :> "get-lesson"         :> GetLesson
           :<|> "library" :> "get-latest-lessons" :> GetLatestLessons
           :<|> "library" :> "search-lessons"     :> SearchLessons

type Login            = Request ReqLogin            RespLogin
type Logout           = Request ReqLogout           OK
type GetUserInfo      = Request ReqGetUserInfo      RespGetUserInfo

type GetLesson        = Request ReqGetLesson        Value

type SearchLessons    = Request ReqSearchLessons    RespSearchLessons
type GetLatestLessons = Request ReqGetLatestLessons RespGetLatestLessons

type Request req resp = ReqBody '[FormUrlEncoded] req :> Post '[JSON] resp

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

data ReqGetLesson = ReqGetLesson {
      reqGetLessonAccessToken :: AccessToken
    , reqGetLessonUserId      :: UserId
    , reqGetLessonV3Id        :: V3Id
    , reqGetLessonType        :: Maybe LessonContent
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

data ReqGetLatestLessons = ReqGetLatestLessons {
      reqGetLatestLessonsAccessToken :: AccessToken
    , reqGetLatestLessonsUserId      :: UserId
    , reqGetLatestLessonsPage        :: Maybe Int
    , reqGetLatestLessonsCount       :: Maybe Int
    , reqGetLatestLessonsLang        :: Maybe String
    , reqGetLatestLessonsLevelId     :: Maybe Level
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Responses
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
    , respLoginAvatarUrl              :: String
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
    , respGetUserInfoAvatarUrl                :: String
    , respGetUserInfoBio                      :: String
    , respGetUserInfoUseTraditionalCharacters :: Bool
    , respGetUserInfoUserId                   :: UserId
    , respGetUserInfoNewLessonNotification    :: Bool
    , respGetUserInfoNewShowNotification      :: Bool
    , respGetUserInfoNewsletterNotification   :: Bool
    , respGetUserInfoGeneralNotification      :: Bool
    , respGetUserInfoLevel                    :: Maybe Level
    , respGetUserInfoType                     :: Undocumented String
    }
  deriving (Show)

type RespSearchLessons    = SearchResults Lesson
type RespGetLatestLessons = SearchResults Lesson

{-------------------------------------------------------------------------------
  Encoding requests
-------------------------------------------------------------------------------}

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
          optFormArg "search_level" (toText . Str) reqSearchLessonsSearchLevel
        , optFormArg "num_results"  (toText)       reqSearchLessonsNumResults
        , optFormArg "page"         (toText)       reqSearchLessonsPage
        ]

instance ToFormUrlEncoded ReqGetLatestLessons where
    toFormUrlEncoded ReqGetLatestLessons{..} = [
          ( "access_token" , toText reqGetLatestLessonsAccessToken )
        , ( "user_id"      , toText reqGetLatestLessonsUserId      )
        ] ++ catMaybes [
          optFormArg "page"     (toText)       reqGetLatestLessonsPage
        , optFormArg "count"    (toText)       reqGetLatestLessonsCount
        , optFormArg "lang"     (toText)       reqGetLatestLessonsLang
        , optFormArg "level_id" (toText . Int) reqGetLatestLessonsLevelId
        ]

instance ToFormUrlEncoded ReqGetLesson where
     toFormUrlEncoded ReqGetLesson{..} = [
          ( "access_token" , toText reqGetLessonAccessToken )
        , ( "user_id"      , toText reqGetLessonUserId      )
        , ( "v3id"         , toText reqGetLessonV3Id        )
        ] ++ catMaybes [
          optFormArg "type" (toText) reqGetLessonType
        ]

optFormArg :: Text -> (a -> Text) -> Maybe a -> Maybe (Text, Text)
optFormArg nm f = fmap $ \a -> (nm, f a)

{-------------------------------------------------------------------------------
  Decoding responses
-------------------------------------------------------------------------------}

instance FromJSON RespLogin where
    parseJSON = withObject "RespLogin" $ \obj -> do
      respLoginAccessToken            <-              obj .: "access_token"
      respLoginUserId                 <-              obj .: "user_id"
      respLoginUsername               <-              obj .: "username"
      respLoginName                   <-              obj .: "name"
      respLoginSelfStudyLessonsTotal  <- strOrInt <$> obj .: "self_study_lessons_total"
      respLoginAssignedLessonsTotal   <-              obj .: "assigned_lessons_total"
      respLoginCoursesCount           <- strOrInt <$> obj .: "courses_count"
      respLoginLang                   <-              obj .: "lang"
      respLoginBio                    <-              obj .: "bio"
      respLoginAvatarUrl              <-              obj .: "avatar_url"
      respLoginNewLessonNotification  <- strOrInt <$> obj .: "new_lesson_notification"
      respLoginNewShowNotification    <- strOrInt <$> obj .: "new_show_notification"
      respLoginNewsletterNotification <- strOrInt <$> obj .: "newsletter_notification"
      respLoginGeneralNotification    <- strOrInt <$> obj .: "general_notification"
      respLoginBookmarkedLessons      <-              obj .: "bookmarked_lessons"
      respLoginSubscribedLessons      <-              obj .: "subscribed_lessons"
      respLoginStudiedLessons         <-              obj .: "studied_lessons"
      return RespLogin{..}

instance FromJSON RespGetUserInfo where
    parseJSON = withObject "RespGetUserInfo" $ \obj -> do
      respGetUserInfoName                     <-              obj .:  "name"
      respGetUserInfoUsername                 <-              obj .:  "username"
      respGetUserInfoAvatarUrl                <-              obj .:  "avatar_url"
      respGetUserInfoBio                      <-              obj .:  "bio"
      respGetUserInfoUseTraditionalCharacters <- strOrInt <$> obj .:  "use_traditional_characters"
      respGetUserInfoUserId                   <- strOrInt <$> obj .:  "user_id"
      respGetUserInfoNewLessonNotification    <- strOrInt <$> obj .:  "new_lesson_notification"
      respGetUserInfoNewShowNotification      <- strOrInt <$> obj .:  "new_show_notification"
      respGetUserInfoNewsletterNotification   <- strOrInt <$> obj .:  "newsletter_notification"
      respGetUserInfoGeneralNotification      <- strOrInt <$> obj .:  "general_notification"
      respGetUserInfoLevel                    <- strOrInt <$> obj .:  "level"
      respGetUserInfoType                     <-              obj .:? "type"
      return RespGetUserInfo{..}

{-------------------------------------------------------------------------------
  ChinesePod specific datatypes
-------------------------------------------------------------------------------}

newtype UserId = UserId String
  deriving (Show, Eq, Ord, FromJSON, ToText, FromText)

newtype AccessToken = AccessToken String
  deriving (Show, Eq, Ord, FromJSON, ToText, FromText)

newtype V3Id = V3Id String
  deriving (Show, Eq, Ord, FromJSON, ToText, FromText)

-- | Some ChinesePod requests simply return OK
data OK = OK
  deriving (Show)

-- | User level
data Level =
    LevelNewbie
  | LevelElementary
  | LevelIntermediate
  | LevelUpperIntermediate
  | LevelAdvanced
  | LevelMedia
  deriving (Show)

data Lesson = Lesson {
      lessonV3Id                 :: V3Id
    , lessonTitle                :: String
    , lessonIntroduction         :: String
    , lessonLevel                :: Maybe Level
    , lessonName                 :: String
    , lessonSlug                 :: String
    , lessonLessonId             :: Maybe String
    , lessonPublicationTimestamp :: String
    , lessonImage                :: String
    , lessonBookMarked           :: Bool
    , lessonMarkAsStudied        :: Bool
    , lessonSource               :: Maybe String
    , lessonStatus               :: Maybe String
    , lessonRadioQualityMp3      :: Maybe String
    , lessonDialogueMp3          :: Maybe String
    , lessonReviewMp3            :: Maybe String
    }
  deriving (Show)

data LessonContent =
    LessonContentAll
  | LessonContentExercise
  | LessonContentVocabulary
  | LessonContentDialogue
  | LessonContentGrammar
  deriving (Show)

{-------------------------------------------------------------------------------
  Encoding/decoding ChinesePod types
-------------------------------------------------------------------------------}

instance FromJSON OK where
    parseJSON = withObject "OK" $ \obj -> do
      result <- obj .: "result"
      case result :: String of
        "OK" -> return OK
        _    -> fail $ "Expected OK"

instance FromJSON Lesson where
    parseJSON = withObject "Lesson" $ \obj -> do
      lessonV3Id                 <-              obj .:  "v3_id"
      lessonTitle                <-              obj .:  "title"
      lessonIntroduction         <-              obj .:  "introduction"
      lessonLevel                <- strOrInt <$> obj .:  "level"
      lessonName                 <-              obj .:  "name"
      lessonSlug                 <-              obj .:  "slug"
      lessonLessonId             <- nullable <$> obj .:? "lesson_id" .!= Nullable Nothing
      lessonPublicationTimestamp <-              obj .:  "publication_timestamp"
      lessonImage                <-              obj .:  "image"
      lessonBookMarked           <- strOrInt <$> obj .:  "book_marked"
      lessonMarkAsStudied        <- strOrInt <$> obj .:  "mark_as_studied"
      lessonSource               <- nullable <$> obj .:? "source" .!= Nullable Nothing
      lessonStatus               <-              obj .:? "status"
      lessonRadioQualityMp3      <-              obj .:? "radio_quality_mp3"
      lessonDialogueMp3          <-              obj .:? "dialogue_mp3"
      lessonReviewMp3            <-              obj .:? "review_mp3"
      return Lesson{..}

instance ToText LessonContent where
    toText LessonContentAll        = "all"
    toText LessonContentExercise   = "exercise"
    toText LessonContentVocabulary = "vocabulary"
    toText LessonContentDialogue   = "dialogue"
    toText LessonContentGrammar    = "grammar"

instance FromText LessonContent where
    fromText "all"        = Just $ LessonContentAll
    fromText "exercise"   = Just $ LessonContentExercise
    fromText "vocabulary" = Just $ LessonContentVocabulary
    fromText "dialogue"   = Just $ LessonContentDialogue
    fromText "grammar"    = Just $ LessonContentGrammar
    fromText _otherwise   = Nothing

{-------------------------------------------------------------------------------
  String/int encoding for specific types
-------------------------------------------------------------------------------}

instance ToStrOrInt Level where
  toStr = go
    where
      go LevelNewbie            = "Newbie"
      go LevelElementary        = "Elementary"
      go LevelIntermediate      = "Intermediate"
      go LevelUpperIntermediate = "Upper Intermediate"
      go LevelAdvanced          = "Advanced"
      go LevelMedia             = "Media"

  toInt = go
    where
      go LevelNewbie            = 1
      go LevelElementary        = 2
      go LevelIntermediate      = 3
      go LevelUpperIntermediate = 4
      go LevelAdvanced          = 5
      go LevelMedia             = 6

instance FromStrOrInt Int where
  fromStr = tryRead . T.unpack
  fromInt = Just

instance FromStrOrInt UserId where
  fromStr = Just . UserId . T.unpack
  fromInt = Just . UserId . show

instance FromStrOrInt Bool where
  fromStr = go
    where
      go "0" = Just False
      go "1" = Just True
      go _   = Nothing

  fromInt = go
    where
      go 0 = Just False
      go 1 = Just True
      go _ = Nothing

instance FromStrOrInt (Maybe Level) where
  fromStr = go
    where
      go "Newbie"             = Just $ Just LevelNewbie
      go "Elementary"         = Just $ Just LevelElementary
      go "Intermediate"       = Just $ Just LevelIntermediate
      go "Upper Intermediate" = Just $ Just LevelUpperIntermediate
      go "Advanced"           = Just $ Just LevelAdvanced
      go "Media"              = Just $ Just LevelMedia
      go _                    = Nothing

  fromInt = go
    where
      go 0 = Just $ Nothing
      go 1 = Just $ Just LevelNewbie
      go 2 = Just $ Just LevelElementary
      go 3 = Just $ Just LevelIntermediate
      go 4 = Just $ Just LevelUpperIntermediate
      go 5 = Just $ Just LevelAdvanced
      go 6 = Just $ Just LevelMedia
      go _ = Nothing

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
  Values that can be encoded as either strings or as numbers
-------------------------------------------------------------------------------}

-- | Encode as either a string or a number
--
-- The ChinesePod API is not very consistent with what is represented as
-- a number, and what as a string. In order not to choke on these, we allow
-- them to be represented as either.
data StrOrInt a =
    Str { strOrInt :: a }
  | Int { strOrInt :: a }

class ToStrOrInt a where
  toStr :: a -> Text
  toInt :: a -> Int

class FromStrOrInt a where
  fromStr :: Text -> Maybe a
  fromInt :: Int  -> Maybe a

instance (Typeable a, FromStrOrInt a) => FromJSON (StrOrInt a) where
    parseJSON (String s) = case fromStr s of
                             Just level -> return $ Str level
                             Nothing    -> parseFailure s
    parseJSON (Number n) = case fromInt (round n) of
                             Just level -> return $ Int level
                             Nothing    -> parseFailure (T.pack $ show n)
    parseJSON val        = typeMismatch (show (typeOf (undefined :: a))) val

instance FromStrOrInt a => FromText (StrOrInt a) where
    fromText txt = maybe (fmap Str $ fromStr txt)
                         (fmap Int . fromInt)
                         (tryRead $ T.unpack txt)

instance ToStrOrInt a => ToText (StrOrInt a) where
  toText (Str a) = toStr a
  toText (Int a) = T.pack $ show (toInt a)

{-------------------------------------------------------------------------------
  Generic search results
-------------------------------------------------------------------------------}

data SearchResults a = SearchResults {
      searchResults      :: Map Int a
    , searchResultsTotal :: Int
    }
  deriving (Show)

instance FromJSON a => FromJSON (SearchResults a) where
    parseJSON = withObject "SearchResults" $ \obj -> do
        let rawResults = catMaybes $ map extractRaw (HashMap.toList obj)
        searchResults      <- Map.fromList <$> mapM parseRaw rawResults
        searchResultsTotal <- strOrInt <$> obj .: "total"
        return SearchResults{..}
      where
        extractRaw :: (Text, Value) -> Maybe (Int, Value)
        extractRaw (idx, val) = do idx' <- fromText idx ; return (idx', val)

        parseRaw :: (Int, Value) -> Parser (Int, a)
        parseRaw (idx, val) = do val' <- parseJSON val ; return (idx, val')

{-------------------------------------------------------------------------------
  Nullable fields (fields whose absence is represented with an explicit 'null')
-------------------------------------------------------------------------------}

newtype Nullable a = Nullable { nullable :: Maybe a }

instance FromJSON a => FromJSON (Nullable a) where
    parseJSON Null = return $ Nullable Nothing
    parseJSON val  = Nullable . Just <$> parseJSON val

{-------------------------------------------------------------------------------
  Undocumented fields
-------------------------------------------------------------------------------}

-- | Some requests return more info than is documented in the API
--
-- Since we should not rely on these fields being set, we mark them.
type Undocumented = Maybe

{-------------------------------------------------------------------------------
  Parser auxiliary
-------------------------------------------------------------------------------}

tryRead :: Read a => String -> Maybe a
tryRead strA =
      case filter fullParse (readsPrec 0 strA) of
        [(a, _)]   -> Just a
        _otherwise -> Nothing
    where
      fullParse :: (a, String) -> Bool
      fullParse = null . snd

parseFailure :: forall a m. (Typeable a, Monad m) => Text -> m a
parseFailure inp = fail $ "Could not parse " ++ show inp ++ " "
                       ++ "as " ++ show (typeOf (undefined :: a))
