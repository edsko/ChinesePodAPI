{-# LANGUAGE OverloadedStrings #-}
module Servant.ChinesePod.API (
    api
    -- * API specification
  , ChinesePod
    -- ** Account
  , Login
  , Logout
  , GetUserInfo
    -- *** Request types
  , ReqLogin(..)
  , ReqLogout(..)
  , ReqGetUserInfo(..)
  , ReqSignature(..)
    -- *** Response types
  , RespLogin(..)
  , RespGetUserInfo(..)
    -- ** Lesson
  , GetLesson
    -- *** Request types
  , ReqGetLesson(..)
    -- ** Library
  , GetLatestLessons
  , SearchLessons
    -- *** Request types
  , ReqGetLatestLessons(..)
  , ReqSearchLessons(..)
    -- *** Response types
  , RespGetLatestLessons
  , RespSearchLessons
    -- * ChinesePod specific datatypes
  , AccessToken(..)
  , Lesson(..)
  , LessonContent(..)
  , LessonContentType(..)
  , Level(..)
  , UserId(..)
  , V3Id(..)
    -- * Auxiliary
    -- ** Types
  , OK(..)
  , Undocumented
  , SearchResults(..)
  , StrOrInt(..)
    -- ** Parsing combinators
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

type GetLesson        = Request ReqGetLesson        LessonContent

type GetLatestLessons = Request ReqGetLatestLessons RespGetLatestLessons
type SearchLessons    = Request ReqSearchLessons    RespSearchLessons

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
    , reqGetLessonType        :: Maybe LessonContentType
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

data LessonContentType =
    LessonContentAll
  | LessonContentExercise
  | LessonContentVocabulary
  | LessonContentDialogue
  | LessonContentGrammar
  deriving (Show)

data LessonContent = LessonContent {
      lessonContentContentId            :: String
    , lessonContentCreatedAt            :: String
    , lessonContentUpdatedAt            :: String
    , lessonContentStatusComments       :: String
    , lessonContentStatusLocked         :: String
    , lessonContentStatusPublished      :: String
    , lessonContentCreatedBy            :: String
    , lessonContentUpdatedBy            :: String
    , lessonContentPopularity           :: String
    , lessonContentRank                 :: String
    , lessonContentSlug                 :: String
    , lessonContentType                 :: String
    , lessonContentSeriesId             :: String
    , lessonContentChannelId            :: String
    , lessonContentMaturity             :: String
    , lessonContentTitle                :: String
    , lessonContentIntroduction         :: String
    , lessonContentTheme                :: String
    , lessonContentChannel              :: String
    , lessonContentLevel                :: Maybe Level
    , lessonContentHosts                :: String
    , lessonContentV3Id                 :: V3Id
    , lessonContentHashCode             :: String
    , lessonContentPublicationTimestamp :: String
    , lessonContentTimeOffset           :: String
    , lessonContentImage                :: String
    , lessonContentText                 :: String
    , lessonContentTranscription1       :: String
    , lessonContentTranscription2       :: String
    , lessonContentMp3Media             :: String
    , lessonContentMp3Mobile            :: String
    , lessonContentPdf1                 :: String
    , lessonContentPdf2                 :: String
    , lessonContentPdf3                 :: String
    , lessonContentPdf4                 :: String
    , lessonContentPpt                  :: Maybe String
    , lessonContentPptSize              :: Maybe String
    , lessonContentVideoFix             :: String
    , lessonContentLinkSource           :: String
    , lessonContentLinkRelated          :: String
    , lessonContentExercisesExercise1   :: String
    , lessonContentExercisesExercise2   :: String
    , lessonContentExercisesExercise3   :: String
    , lessonContentExercisesExercise4   :: String
    , lessonContentXmlFileName          :: String
    , lessonContentMp3DialogueSize      :: Int
    , lessonContentMp3MediaSize         :: Int
    , lessonContentMp3MobileSize        :: Int
    , lessonContentMp3PublicSize        :: Int
    , lessonContentMp3PrivateSize       :: Int
    , lessonContentMp3ThefixSize        :: Int
    , lessonContentMp3ThefixLength      :: String
    , lessonContentMp3PublicLength      :: String
    , lessonContentMp3PrivateLength     :: String
    , lessonContentMp3MobileLength      :: String
    , lessonContentMp3MediaLength       :: String
    , lessonContentMp3DialogueLength    :: String
    , lessonContentVideoFlv             :: String
    , lessonContentVideoFlvSize         :: Int
    , lessonContentVideoFlvLength       :: String
    , lessonContentVideoMp4             :: String
    , lessonContentVideoMp4Size         :: Int
    , lessonContentVideoMp4Length       :: String
    , lessonContentVideoM4v             :: String
    , lessonContentVideoM4vSize         :: Int
    , lessonContentVideoM4vLength       :: String
    , lessonContentLastCommentId        :: String
    , lessonContentLastCommentTime      :: String
    , lessonContentIsPrivate            :: Bool
    , lessonContentVideo                :: Maybe String
    , lessonContentLessonPlan           :: String
    , lessonContentLessonAssignment     :: String
    , lessonContentName                 :: String
    , lessonContentSeriesName           :: String
    , lessonContentRadioQualityMp3      :: String
    , lessonContentCdQualityMp3         :: String
    , lessonContentDialogueMp3          :: String
    , lessonContentReviewMp3            :: String
    , lessonContentCommentCount         :: Int
    , lessonContentVideoLesson          :: Bool
    , lessonContentAccessLevel          :: String
    , lessonContentBookMarked           :: Bool
    , lessonContentMarkAsStudied        :: Bool
    , lessonContentStudentFullname      :: String
    , lessonContentPostDate             :: Maybe String
    , lessonContentStudentComment       :: Maybe String
    , lessonContentFileName             :: String
    , lessonContentFileUrl              :: Maybe String
    , lessonContentTeacherName          :: Maybe String
    , lessonContentTeacherId            :: Maybe String
    , lessonContentReviewDate           :: Maybe String
    , lessonContentTeacherFeedback      :: Maybe String
    }
  deriving (Show)

instance FromJSON LessonContent where
    parseJSON = withObject "LessonContent" $ \obj -> do
      lessonContentContentId            <-              obj .:  "content_id"
      lessonContentCreatedAt            <-              obj .:  "created_at"
      lessonContentUpdatedAt            <-              obj .:  "updated_at"
      lessonContentStatusComments       <-              obj .:  "status_comments"
      lessonContentStatusLocked         <-              obj .:  "status_locked"
      lessonContentStatusPublished      <-              obj .:  "status_published"
      lessonContentCreatedBy            <-              obj .:  "created_by"
      lessonContentUpdatedBy            <-              obj .:  "updated_by"
      lessonContentPopularity           <-              obj .:  "popularity"
      lessonContentRank                 <-              obj .:  "rank"
      lessonContentSlug                 <-              obj .:  "slug"
      lessonContentType                 <-              obj .:  "type"
      lessonContentSeriesId             <-              obj .:  "series_id"
      lessonContentChannelId            <-              obj .:  "channel_id"
      lessonContentMaturity             <-              obj .:  "maturity"
      lessonContentTitle                <-              obj .:  "title"
      lessonContentIntroduction         <-              obj .:  "introduction"
      lessonContentTheme                <-              obj .:  "theme"
      lessonContentChannel              <-              obj .:  "channel"
      lessonContentLevel                <- strOrInt <$> obj .:  "level"
      lessonContentHosts                <-              obj .:  "hosts"
      lessonContentV3Id                 <-              obj .:  "v3_id"
      lessonContentHashCode             <-              obj .:  "hash_code"
      lessonContentPublicationTimestamp <-              obj .:  "publication_timestamp"
      lessonContentTimeOffset           <-              obj .:  "time_offset"
      lessonContentImage                <-              obj .:  "image"
      lessonContentText                 <-              obj .:  "text"
      lessonContentTranscription1       <-              obj .:  "transcription1"
      lessonContentTranscription2       <-              obj .:  "transcription2"
      lessonContentMp3Media             <-              obj .:  "mp3_media"
      lessonContentMp3Mobile            <-              obj .:  "mp3_mobile"
      lessonContentPdf1                 <-              obj .:  "pdf1"
      lessonContentPdf2                 <-              obj .:  "pdf2"
      lessonContentPdf3                 <-              obj .:  "pdf3"
      lessonContentPdf4                 <-              obj .:  "pdf4"
      lessonContentPpt                  <- nullable <$> obj .:? "ppt"      .!= Nullable Nothing
      lessonContentPptSize              <- nullable <$> obj .:? "ppt_size" .!= Nullable Nothing
      lessonContentVideoFix             <-              obj .:  "video_fix"
      lessonContentLinkSource           <-              obj .:  "link_source"
      lessonContentLinkRelated          <-              obj .:  "link_related"
      lessonContentExercisesExercise1   <-              obj .:  "exercises_exercise1"
      lessonContentExercisesExercise2   <-              obj .:  "exercises_exercise2"
      lessonContentExercisesExercise3   <-              obj .:  "exercises_exercise3"
      lessonContentExercisesExercise4   <-              obj .:  "exercises_exercise4"
      lessonContentXmlFileName          <-              obj .:  "xml_file_name"
      lessonContentMp3DialogueSize      <- strOrInt <$> obj .:  "mp3_dialogue_size"
      lessonContentMp3MediaSize         <- strOrInt <$> obj .:  "mp3_media_size"
      lessonContentMp3MobileSize        <- strOrInt <$> obj .:  "mp3_mobile_size"
      lessonContentMp3PublicSize        <- strOrInt <$> obj .:  "mp3_public_size"
      lessonContentMp3PrivateSize       <- strOrInt <$> obj .:  "mp3_private_size"
      lessonContentMp3ThefixSize        <- strOrInt <$> obj .:  "mp3_thefix_size"
      lessonContentMp3ThefixLength      <-              obj .:  "mp3_thefix_length"
      lessonContentMp3PublicLength      <-              obj .:  "mp3_public_length"
      lessonContentMp3PrivateLength     <-              obj .:  "mp3_private_length"
      lessonContentMp3MobileLength      <-              obj .:  "mp3_mobile_length"
      lessonContentMp3MediaLength       <-              obj .:  "mp3_media_length"
      lessonContentMp3DialogueLength    <-              obj .:  "mp3_dialogue_length"
      lessonContentVideoFlv             <-              obj .:  "video_flv"
      lessonContentVideoFlvSize         <- strOrInt <$> obj .:  "video_flv_size"
      lessonContentVideoFlvLength       <-              obj .:  "video_flv_length"
      lessonContentVideoMp4             <-              obj .:  "video_mp4"
      lessonContentVideoMp4Size         <- strOrInt <$> obj .:  "video_mp4_size"
      lessonContentVideoMp4Length       <-              obj .:  "video_mp4_length"
      lessonContentVideoM4v             <-              obj .:  "video_m4v"
      lessonContentVideoM4vSize         <- strOrInt <$> obj .:  "video_m4v_size"
      lessonContentVideoM4vLength       <-              obj .:  "video_m4v_length"
      lessonContentLastCommentId        <-              obj .:  "last_comment_id"
      lessonContentLastCommentTime      <-              obj .:  "last_comment_time"
      lessonContentIsPrivate            <- strOrInt <$> obj .:  "is_private"
      lessonContentVideo                <- nullable <$> obj .:? "video" .!= Nullable Nothing
      lessonContentLessonPlan           <-              obj .:  "lesson_plan"
      lessonContentLessonAssignment     <-              obj .:  "lesson_assignment"
      lessonContentName                 <-              obj .:  "name"
      lessonContentSeriesName           <-              obj .:  "series_name"
      lessonContentRadioQualityMp3      <-              obj .:  "radio_quality_mp3"
      lessonContentCdQualityMp3         <-              obj .:  "cd_quality_mp3"
      lessonContentDialogueMp3          <-              obj .:  "dialogue_mp3"
      lessonContentReviewMp3            <-              obj .:  "review_mp3"
      lessonContentCommentCount         <- strOrInt <$> obj .:  "comment_count"
      lessonContentVideoLesson          <-              obj .:  "video_lesson"
      lessonContentAccessLevel          <-              obj .:  "access_level"
      lessonContentBookMarked           <- strOrInt <$> obj .:  "book_marked"
      lessonContentMarkAsStudied        <- strOrInt <$> obj .:  "mark_as_studied"
      lessonContentStudentFullname      <-              obj .:  "student_fullname"
      lessonContentPostDate             <- nullable <$> obj .:? "post_date"        .!= Nullable Nothing
      lessonContentStudentComment       <- nullable <$> obj .:? "student_comment"  .!= Nullable Nothing
      lessonContentFileName             <-              obj .:  "file_name"
      lessonContentFileUrl              <- nullable <$> obj .:? "file_url"         .!= Nullable Nothing
      lessonContentTeacherName          <- nullable <$> obj .:? "teacher_name"     .!= Nullable Nothing
      lessonContentTeacherId            <-              obj .:  "teacher_id"
      lessonContentReviewDate           <- nullable <$> obj .:? "review_date"      .!= Nullable Nothing
      lessonContentTeacherFeedback      <- nullable <$> obj .:? "teacher_feedback" .!= Nullable Nothing
      return LessonContent{..}

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

instance ToText LessonContentType where
    toText LessonContentAll        = "all"
    toText LessonContentExercise   = "exercise"
    toText LessonContentVocabulary = "vocabulary"
    toText LessonContentDialogue   = "dialogue"
    toText LessonContentGrammar    = "grammar"

instance FromText LessonContentType where
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
