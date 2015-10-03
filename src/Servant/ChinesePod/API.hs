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
    -- * ChinesePod specific datatypes
  , AccessToken(..)
  , Example
  , Expansion(..)
  , GrammarPoint(..)
  , GrammarSentence(..)
  , Lesson(..)
  , LessonContent(..)
  , LessonContentType(..)
  , Level(..)
  , Sentence(..)
  , UserId(..)
  , V3Id(..)
  , Vocabulary(..)
  , Word(..)
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

import Prelude hiding (Word)
import Control.Monad
import Crypto.Hash
import Data.Aeson.Types hiding ((.:?))
import Data.Binary (Binary)
import Data.Data (Data)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Servant.API
import Text.Show.Pretty (PrettyVal(..))
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS.UTF8
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Map             as Map
import qualified Data.Text            as T
import qualified Data.Vector          as Vector

import Servant.ChinesePod.Util.Orphans.PrettyVal ()

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

type GetLatestLessons = Request ReqGetLatestLessons (SearchResults Lesson)
type SearchLessons    = Request ReqSearchLessons    (SearchResults Lesson)

type Request req resp = ReqBody '[FormUrlEncoded] req :> Post '[JSON] resp

{-------------------------------------------------------------------------------
  Request types
-------------------------------------------------------------------------------}

data ReqLogin = ReqLogin {
      reqLoginClientId  :: String
    , reqLoginEmail     :: String
    , reqLoginSignature :: ReqSignature
    }
  deriving (Show, Generic, Data)

data ReqSignature = ReqSignature {
      reqSignatureClientSecret :: String
    , reqSignatureUserPassword :: String
    }
  deriving (Show, Generic, Data)

data ReqLogout = ReqLogout {
      reqLogoutAccessToken :: AccessToken
    , reqLogoutUserId      :: UserId
    }
  deriving (Show, Generic, Data)

data ReqGetUserInfo = ReqGetUserInfo {
      reqGetUserInfoAccessToken :: AccessToken
    , reqGetUserInfoUserId      :: UserId
    }
  deriving (Show, Generic, Data)

data ReqGetLesson = ReqGetLesson {
      reqGetLessonAccessToken :: AccessToken
    , reqGetLessonUserId      :: UserId
    , reqGetLessonV3Id        :: V3Id
    , reqGetLessonType        :: Maybe LessonContentType
    }
  deriving (Show, Generic, Data)

data ReqSearchLessons = ReqSearchLessons {
      reqSearchLessonsAccessToken :: AccessToken
    , reqSearchLessonsUserId      :: UserId
    , reqSearchLessonsSearch      :: String
    , reqSearchLessonsSearchLevel :: Maybe Level
    , reqSearchLessonsNumResults  :: Maybe Int
    , reqSearchLessonsPage        :: Maybe Int
    }
  deriving (Show, Generic, Data)

data ReqGetLatestLessons = ReqGetLatestLessons {
      reqGetLatestLessonsAccessToken :: AccessToken
    , reqGetLatestLessonsUserId      :: UserId
    , reqGetLatestLessonsPage        :: Maybe Int
    , reqGetLatestLessonsCount       :: Maybe Int
    , reqGetLatestLessonsLang        :: Maybe String
    , reqGetLatestLessonsLevelId     :: Maybe Level
    }
  deriving (Show, Generic, Data)

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
  deriving (Show, Generic, Data)

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
  deriving (Show, Generic, Data)

{-------------------------------------------------------------------------------
  ChinesePod specific datatypes
-------------------------------------------------------------------------------}

newtype UserId = UserId { userIdString :: String }
  deriving (Show, Generic, Data, Eq, Ord, FromJSON, ToText, FromText, IsString)

newtype AccessToken = AccessToken { accessTokenString :: String }
  deriving (Show, Generic, Data, Eq, Ord, FromJSON, ToText, FromText, IsString)

newtype V3Id = V3Id { v3IdString :: String }
  deriving (Show, Generic, Data, Eq, Ord, FromJSON, ToText, FromText, IsString)

-- | Some ChinesePod requests simply return OK
data OK = OK
  deriving (Show, Generic, Data)

-- | User level
data Level =
    LevelNewbie
  | LevelElementary
  | LevelIntermediate
  | LevelUpperIntermediate
  | LevelAdvanced
  | LevelMedia
  | LevelOther String
  deriving (Show, Generic, Data)

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
  deriving (Show, Generic, Data)

data LessonContentType =
    LessonContentAll
  | LessonContentExercise
  | LessonContentVocabulary
  | LessonContentDialogue
  | LessonContentGrammar
  deriving (Show, Generic, Data)

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
    , lessonContentCdQualityMp3         :: Maybe String
    , lessonContentDialogueMp3          :: Maybe String
    , lessonContentReviewMp3            :: Maybe String
    , lessonContentCommentCount         :: Int
    , lessonContentVideoLesson          :: Maybe Bool
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
    , lessonContentTopics               :: [String]
    , lessonContentFunctions            :: [String]
    , lessonContentDialogue             :: Maybe [Sentence]
    , lessonContentGrammar              :: Maybe [GrammarPoint]
    , lessonContentExpansion            :: Maybe Expansion
    , lessonContentVocabulary           :: Maybe Vocabulary
    }
  deriving (Show, Generic, Data)

data Sentence = Sentence {
      sentenceV3Id          :: V3Id
    , sentenceAudio         :: String
    , sentenceDisplayOrder  :: Int
    , sentenceId            :: String
    , sentencePinyin        :: String
    , sentenceRow3          :: String
    , sentenceRow4          :: String
    , sentenceSource        :: String
    , sentenceSourceT       :: Maybe String
    , sentenceSpeaker       :: String
    , sentenceTarget        :: String
    , sentenceVocabulary    :: String
    , sentenceSentenceWords :: [Word]
    }
  deriving (Show, Generic, Data)

data Word = Word {
      wordV3Id            :: Maybe V3Id
    , wordAudio           :: Maybe String
    , wordId              :: Maybe String
    , wordPinyin          :: String
    , wordSource          :: String
    , wordSourceT         :: String
    , wordTarget          :: String
    , wordVcid            :: Maybe String
    , wordImage           :: Maybe String
    , wordDisplayOrder    :: Maybe Int
    , wordVocabularyClass :: Maybe String
    }
  deriving (Show, Generic, Data)

data GrammarPoint = GrammarPoint {
      grammarPointCreateTime     :: String
    , grammarPointDisplayLayer   :: Int
    , grammarPointDisplaySort    :: Int
    , grammarPointDisplayType    :: String
    , grammarPointGrammarId      :: String
    , grammarPointImage          :: String
    , grammarPointIntroduction   :: String
    , grammarPointLevel          :: Maybe Level
    , grammarPointName           :: String
    , grammarPointParentId       :: String
    , grammarPointPath           :: String
    , grammarPointProductionId   :: String
    , grammarPointRelatedGrammar :: String
    , grammarPointSentences      :: [GrammarSentence]
    , grammarPointSummary        :: String
    , grammarPointTree           :: String
    , grammarPointUpdateTime     :: String
    }
  deriving (Show, Generic, Data)

data GrammarSentence = GrammarSentence {
      grammarSentenceAudio             :: String
    , grammarSentenceCreateTime        :: Maybe String
    , grammarSentenceDescription       :: String
    , grammarSentenceDisplaySort       :: Maybe Int
    , grammarSentenceGrammarBlockId    :: Maybe String
    , grammarSentenceGrammarId         :: String
    , grammarSentenceGrammarSentenceId :: Maybe String
    , grammarSentenceIsCorrect         :: Maybe Bool
    , grammarSentencePinyin            :: String
    , grammarSentenceSource            :: String
    , grammarSentenceSourceAudio       :: Maybe String
    , grammarSentenceSourceT           :: String
    , grammarSentenceSourceTrad        :: Maybe String
    , grammarSentenceSummary           :: String
    , grammarSentenceTarget            :: Maybe String
    , grammarSentenceTargetAnnotate    :: Maybe String
    , grammarSentenceTargetAudio       :: Maybe String
    , grammarSentenceTargetTrad        :: Maybe String
    , grammarSentenceTips              :: Maybe String
    , grammarSentenceUpdateTime        :: Maybe String
    , grammarSentenceWords             :: [Word]
    }
  deriving (Show, Generic, Data)

data Example = Example {
      exampleAudio         :: String
    , exampleExpansionWord :: [Word]
    , exampleId            :: String
    , examplePinyin        :: String
    , exampleSource        :: String
    , exampleSourceT       :: Maybe String
    , exampleTarget        :: String
    }
  deriving (Show, Generic, Data)

data Vocabulary = Vocabulary {
      vocabularyKeyVocab :: [Word]
    , vocabularySupVocab :: [Word]
    }
  deriving (Show, Generic, Data)

data Expansion = Expansion {
    expansion :: Map String [Example]
  }
  deriving (Show, Generic, Data)

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
      respLoginAccessToken            <- obj .:  "access_token"
      respLoginUserId                 <- obj .:  "user_id"
      respLoginUsername               <- obj .:  "username"
      respLoginName                   <- obj .:  "name"
      respLoginSelfStudyLessonsTotal  <- obj .:~ "self_study_lessons_total"
      respLoginAssignedLessonsTotal   <- obj .:  "assigned_lessons_total"
      respLoginCoursesCount           <- obj .:~ "courses_count"
      respLoginLang                   <- obj .:  "lang"
      respLoginBio                    <- obj .:  "bio"
      respLoginAvatarUrl              <- obj .:  "avatar_url"
      respLoginNewLessonNotification  <- obj .:~ "new_lesson_notification"
      respLoginNewShowNotification    <- obj .:~ "new_show_notification"
      respLoginNewsletterNotification <- obj .:~ "newsletter_notification"
      respLoginGeneralNotification    <- obj .:~ "general_notification"
      respLoginBookmarkedLessons      <- obj .:  "bookmarked_lessons"
      respLoginSubscribedLessons      <- obj .:  "subscribed_lessons"
      respLoginStudiedLessons         <- obj .:  "studied_lessons"
      return RespLogin{..}

instance FromJSON RespGetUserInfo where
    parseJSON = withObject "RespGetUserInfo" $ \obj -> do
      respGetUserInfoName                     <- obj .:  "name"
      respGetUserInfoUsername                 <- obj .:  "username"
      respGetUserInfoAvatarUrl                <- obj .:  "avatar_url"
      respGetUserInfoBio                      <- obj .:  "bio"
      respGetUserInfoUseTraditionalCharacters <- obj .:~ "use_traditional_characters"
      respGetUserInfoUserId                   <- obj .:~ "user_id"
      respGetUserInfoNewLessonNotification    <- obj .:~ "new_lesson_notification"
      respGetUserInfoNewShowNotification      <- obj .:~ "new_show_notification"
      respGetUserInfoNewsletterNotification   <- obj .:~ "newsletter_notification"
      respGetUserInfoGeneralNotification      <- obj .:~ "general_notification"
      respGetUserInfoLevel                    <- obj .:~ "level"
      respGetUserInfoType                     <- obj .:? "type"
      return RespGetUserInfo{..}

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
      lessonV3Id                 <- obj .:  "v3_id"
      lessonTitle                <- obj .:  "title"
      lessonIntroduction         <- obj .:  "introduction"
      lessonLevel                <- obj .:~ "level"
      lessonName                 <- obj .:  "name"
      lessonSlug                 <- obj .:  "slug"
      lessonLessonId             <- obj .:? "lesson_id"
      lessonPublicationTimestamp <- obj .:  "publication_timestamp"
      lessonImage                <- obj .:  "image"
      lessonBookMarked           <- obj .:~ "book_marked"
      lessonMarkAsStudied        <- obj .:~ "mark_as_studied"
      lessonSource               <- obj .:? "source"
      lessonStatus               <- obj .:? "status"
      lessonRadioQualityMp3      <- obj .:? "radio_quality_mp3"
      lessonDialogueMp3          <- obj .:? "dialogue_mp3"
      lessonReviewMp3            <- obj .:? "review_mp3"
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

instance FromJSON LessonContent where
    parseJSON = withObject "LessonContent" $ \obj -> do
      lessonContentContentId            <- obj .:  "content_id"
      lessonContentCreatedAt            <- obj .:  "created_at"
      lessonContentUpdatedAt            <- obj .:  "updated_at"
      lessonContentStatusComments       <- obj .:  "status_comments"
      lessonContentStatusLocked         <- obj .:  "status_locked"
      lessonContentStatusPublished      <- obj .:  "status_published"
      lessonContentCreatedBy            <- obj .:  "created_by"
      lessonContentUpdatedBy            <- obj .:  "updated_by"
      lessonContentPopularity           <- obj .:  "popularity"
      lessonContentRank                 <- obj .:  "rank"
      lessonContentSlug                 <- obj .:  "slug"
      lessonContentType                 <- obj .:  "type"
      lessonContentSeriesId             <- obj .:  "series_id"
      lessonContentChannelId            <- obj .:  "channel_id"
      lessonContentMaturity             <- obj .:  "maturity"
      lessonContentTitle                <- obj .:  "title"
      lessonContentIntroduction         <- obj .:  "introduction"
      lessonContentTheme                <- obj .:  "theme"
      lessonContentChannel              <- obj .:  "channel"
      lessonContentLevel                <- obj .:~ "level"
      lessonContentHosts                <- obj .:  "hosts"
      lessonContentV3Id                 <- obj .:  "v3_id"
      lessonContentHashCode             <- obj .:  "hash_code"
      lessonContentPublicationTimestamp <- obj .:  "publication_timestamp"
      lessonContentTimeOffset           <- obj .:  "time_offset"
      lessonContentImage                <- obj .:  "image"
      lessonContentText                 <- obj .:  "text"
      lessonContentTranscription1       <- obj .:  "transcription1"
      lessonContentTranscription2       <- obj .:  "transcription2"
      lessonContentMp3Media             <- obj .:  "mp3_media"
      lessonContentMp3Mobile            <- obj .:  "mp3_mobile"
      lessonContentPdf1                 <- obj .:  "pdf1"
      lessonContentPdf2                 <- obj .:  "pdf2"
      lessonContentPdf3                 <- obj .:  "pdf3"
      lessonContentPdf4                 <- obj .:  "pdf4"
      lessonContentPpt                  <- obj .:? "ppt"
      lessonContentPptSize              <- obj .:? "ppt_size"
      lessonContentVideoFix             <- obj .:  "video_fix"
      lessonContentLinkSource           <- obj .:  "link_source"
      lessonContentLinkRelated          <- obj .:  "link_related"
      lessonContentExercisesExercise1   <- obj .:  "exercises_exercise1"
      lessonContentExercisesExercise2   <- obj .:  "exercises_exercise2"
      lessonContentExercisesExercise3   <- obj .:  "exercises_exercise3"
      lessonContentExercisesExercise4   <- obj .:  "exercises_exercise4"
      lessonContentXmlFileName          <- obj .:  "xml_file_name"
      lessonContentMp3DialogueSize      <- obj .:~ "mp3_dialogue_size"
      lessonContentMp3MediaSize         <- obj .:~ "mp3_media_size"
      lessonContentMp3MobileSize        <- obj .:~ "mp3_mobile_size"
      lessonContentMp3PublicSize        <- obj .:~ "mp3_public_size"
      lessonContentMp3PrivateSize       <- obj .:~ "mp3_private_size"
      lessonContentMp3ThefixSize        <- obj .:~ "mp3_thefix_size"
      lessonContentMp3ThefixLength      <- obj .:  "mp3_thefix_length"
      lessonContentMp3PublicLength      <- obj .:  "mp3_public_length"
      lessonContentMp3PrivateLength     <- obj .:  "mp3_private_length"
      lessonContentMp3MobileLength      <- obj .:  "mp3_mobile_length"
      lessonContentMp3MediaLength       <- obj .:  "mp3_media_length"
      lessonContentMp3DialogueLength    <- obj .:  "mp3_dialogue_length"
      lessonContentVideoFlv             <- obj .:  "video_flv"
      lessonContentVideoFlvSize         <- obj .:~ "video_flv_size"
      lessonContentVideoFlvLength       <- obj .:  "video_flv_length"
      lessonContentVideoMp4             <- obj .:  "video_mp4"
      lessonContentVideoMp4Size         <- obj .:~ "video_mp4_size"
      lessonContentVideoMp4Length       <- obj .:  "video_mp4_length"
      lessonContentVideoM4v             <- obj .:  "video_m4v"
      lessonContentVideoM4vSize         <- obj .:~ "video_m4v_size"
      lessonContentVideoM4vLength       <- obj .:  "video_m4v_length"
      lessonContentLastCommentId        <- obj .:  "last_comment_id"
      lessonContentLastCommentTime      <- obj .:  "last_comment_time"
      lessonContentIsPrivate            <- obj .:~ "is_private"
      lessonContentVideo                <- obj .:? "video"
      lessonContentLessonPlan           <- obj .:  "lesson_plan"
      lessonContentLessonAssignment     <- obj .:  "lesson_assignment"
      lessonContentName                 <- obj .:  "name"
      lessonContentSeriesName           <- obj .:  "series_name"
      lessonContentRadioQualityMp3      <- obj .:  "radio_quality_mp3"
      lessonContentCdQualityMp3         <- obj .:? "cd_quality_mp3"
      lessonContentDialogueMp3          <- obj .:? "dialogue_mp3"
      lessonContentReviewMp3            <- obj .:? "review_mp3"
      lessonContentCommentCount         <- obj .:~ "comment_count"
      lessonContentVideoLesson          <- obj .:? "video_lesson"
      lessonContentAccessLevel          <- obj .:  "access_level"
      lessonContentBookMarked           <- obj .:~ "book_marked"
      lessonContentMarkAsStudied        <- obj .:~ "mark_as_studied"
      lessonContentStudentFullname      <- obj .:  "student_fullname"
      lessonContentPostDate             <- obj .:? "post_date"
      lessonContentStudentComment       <- obj .:? "student_comment"
      lessonContentFileName             <- obj .:  "file_name"
      lessonContentFileUrl              <- obj .:? "file_url"
      lessonContentTeacherName          <- obj .:? "teacher_name"
      lessonContentTeacherId            <- obj .:  "teacher_id"
      lessonContentReviewDate           <- obj .:? "review_date"
      lessonContentTeacherFeedback      <- obj .:? "teacher_feedback"
      lessonContentTopics               <- obj .:  "topics"
      lessonContentFunctions            <- obj .:  "functions"
      lessonContentDialogue             <- obj .:? "dialogue"
      lessonContentGrammar              <- obj .:? "grammar"
      lessonContentExpansion            <- obj .:? "expansion"
      lessonContentVocabulary           <- obj .:? "vocabulary"
      return LessonContent{..}

instance FromJSON Sentence where
    parseJSON = withObject "Sentence" $ \obj -> do
      sentenceV3Id          <- obj .:  "v3_id"
      sentenceAudio         <- obj .:  "audio"
      sentenceDisplayOrder  <- obj .:~ "display_order"
      sentenceId            <- obj .:  "id"
      sentencePinyin        <- obj .:  "pinyin"
      sentenceRow3          <- obj .:  "row_3"
      sentenceRow4          <- obj .:  "row_4"
      sentenceSource        <- obj .:  "source"
      sentenceSourceT       <- obj .:? "source_t"
      sentenceSpeaker       <- obj .:  "speaker"
      sentenceTarget        <- obj .:  "target"
      sentenceVocabulary    <- obj .:  "vocabulary"
      sentenceSentenceWords <- obj .:  "sentence_words"
      return Sentence{..}

instance FromJSON Word where
    parseJSON = withObject "Word" $ \obj -> do
      wordV3Id            <- obj .:?  "v3_id"
      wordAudio           <- obj .:?  "audio"
      wordId              <- obj .:?  "id"
      wordPinyin          <- obj .:   "pinyin"
      wordSource          <- obj .:   "source"
      wordSourceT         <- obj .:   "source_t"
      wordTarget          <- obj .:   "target"
      wordVcid            <- obj .:?  "vcid"
      wordImage           <- obj .:?  "image"
      wordDisplayOrder    <- obj .:?~ "display_order"
      wordVocabularyClass <- obj .:?  "vocabulary_class"
      return Word{..}

instance FromJSON GrammarPoint where
    parseJSON = withObject "GrammarPoint" $ \obj -> do
      grammarPointCreateTime     <- obj .:  "create_time"
      grammarPointDisplayLayer   <- obj .:~ "display_layer"
      grammarPointDisplaySort    <- obj .:~ "display_sort"
      grammarPointDisplayType    <- obj .:  "display_type"
      grammarPointGrammarId      <- obj .:  "grammar_id"
      grammarPointImage          <- obj .:  "image"
      grammarPointIntroduction   <- obj .:  "introduction"
      grammarPointLevel          <- join <$> obj .:?~ "level_name"
      grammarPointName           <- obj .:  "name"
      grammarPointParentId       <- obj .:  "parent_id"
      grammarPointPath           <- obj .:  "path"
      grammarPointProductionId   <- obj .:  "production_id"
      grammarPointRelatedGrammar <- obj .:  "related_grammar"
      grammarPointSentences      <- obj .:  "sentences"
      grammarPointSummary        <- obj .:  "summary"
      grammarPointTree           <- obj .:  "tree"
      grammarPointUpdateTime     <- obj .:  "update_time"
      return GrammarPoint{..}

instance FromJSON GrammarSentence where
    parseJSON = withObject "GrammarSentence" $ \obj -> do
      grammarSentenceAudio             <- obj .:   "audio"
      grammarSentenceCreateTime        <- obj .:?  "create_time"
      grammarSentenceDescription       <- obj .:   "description"
      grammarSentenceDisplaySort       <- obj .:?~ "display_sort"
      grammarSentenceGrammarBlockId    <- obj .:?  "grammar_block_id"
      grammarSentenceGrammarId         <- obj .:   "grammar_id"
      grammarSentenceGrammarSentenceId <- obj .:?  "grammar_sentence_id"
      grammarSentenceIsCorrect         <- obj .:?~ "is_correct"
      grammarSentencePinyin            <- obj .:   "pinyin"
      grammarSentenceSource            <- obj .:   "source"
      grammarSentenceSourceAudio       <- obj .:?  "source_audio"
      grammarSentenceSourceT           <- obj .:   "source_t"
      grammarSentenceSourceTrad        <- obj .:?  "source_trad"
      grammarSentenceSummary           <- obj .:   "summary"
      grammarSentenceTarget            <- obj .:?  "target"
      grammarSentenceTargetAnnotate    <- obj .:?  "target_annotate"
      grammarSentenceTargetAudio       <- obj .:?  "target_audio"
      grammarSentenceTargetTrad        <- obj .:?  "target_trad"
      grammarSentenceTips              <- obj .:?  "tips"
      grammarSentenceUpdateTime        <- obj .:?  "update_time"
      grammarSentenceWords             <- obj .:   "words"
      return GrammarSentence{..}

instance FromJSON Example where
    parseJSON = withObject "Example" $ \obj -> do
      exampleAudio         <- obj .:  "audio"
      exampleExpansionWord <- maybeIndexed <$> obj .:  "expansion_word"
      exampleId            <- obj .:  "id"
      examplePinyin        <- obj .:  "pinyin"
      exampleSource        <- obj .:  "source"
      exampleSourceT       <- obj .:? "source_t"
      exampleTarget        <- obj .:  "target"
      return Example{..}

instance FromJSON Vocabulary where
    parseJSON = withObject "Vocabulary" $ \obj -> do
      vocabularyKeyVocab <- obj .: "key_vocab"
      vocabularySupVocab <- obj .: "sup_vocab"
      return Vocabulary{..}

instance FromJSON Expansion where
     parseJSON (Object obj) =
         Expansion . Map.fromList <$> mapM parseField (HashMap.toList obj)
       where
         parseField :: (Text, Value) -> Parser (String, [Example])
         parseField (word, val) = do
           examples <- parseJSON val
           return (T.unpack word, examples)

     parseJSON (Array arr) = do
         if Vector.null arr
           then return Expansion { expansion = Map.empty }
           else fail $ "Unexpected non-empty array in 'expansion'"

     parseJSON val =
         typeMismatch "Expansion" val

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
      go (LevelOther other)     = T.pack other

  toInt = go
    where
      go LevelNewbie            = 1
      go LevelElementary        = 2
      go LevelIntermediate      = 3
      go LevelUpperIntermediate = 4
      go LevelAdvanced          = 5
      go LevelMedia             = 6
      go (LevelOther other)     = error $ "No numeric value for " ++ other

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
      go other                = Just $ Just (LevelOther $ T.unpack other)

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
  deriving (Show, Generic, Data)

instance FromJSON a => FromJSON (SearchResults a) where
    parseJSON = withObject "SearchResults" $ \obj -> do
        let rawResults = catMaybes $ map extractRaw (HashMap.toList obj)
        searchResults      <- Map.fromList <$> mapM parseRaw rawResults
        searchResultsTotal <- obj .:~ "total"
        return SearchResults{..}
      where
        extractRaw :: (Text, Value) -> Maybe (Int, Value)
        extractRaw (idx, val) = do idx' <- fromText idx ; return (idx', val)

        parseRaw :: (Int, Value) -> Parser (Int, a)
        parseRaw (idx, val) = do val' <- parseJSON val ; return (idx, val')

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

-- | Variant on '(Aeson..:?)' which regards 'Null' as absent, too.
(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
obj .:? key = case HashMap.lookup key obj of
                Just Null  -> return Nothing
                _otherwise -> obj Aeson..:? key

-- | "Approximate" accessor that uses StrOrInt
(.:~) :: (Typeable a, FromStrOrInt a) => Object -> Text -> Parser a
obj .:~ key = strOrInt <$> obj .: key

-- | Combination of '(.:?)' and '(.:~)'
(.:?~) :: (Typeable a, FromStrOrInt a) => Object -> Text -> Parser (Maybe a)
obj .:?~ key = fmap strOrInt <$> obj .:? key

-- | A list that is either represented as a JSON list or as a JSON object with
-- indices as keys
newtype MaybeIndexed a = MaybeIndexed { maybeIndexed :: [a] }

instance (Typeable a, FromJSON a) => FromJSON (MaybeIndexed a) where
    parseJSON (Array arr) =
        MaybeIndexed <$> mapM parseJSON (Vector.toList arr)

    parseJSON (Object obj) = do
        let rawResults = catMaybes $ map extractRaw (HashMap.toList obj)
        MaybeIndexed <$> mapM parseJSON (sortRaw rawResults)
      where
        extractRaw :: (Text, Value) -> Maybe (Int, Value)
        extractRaw (idx, val) = do idx' <- fromText idx ; return (idx', val)

        sortRaw :: [(Int, Value)] -> [Value]
        sortRaw = map snd . sortBy (comparing fst)

    parseJSON val =
        typeMismatch ("MaybeIndex " ++ show (typeOf (undefined :: a))) val

{-------------------------------------------------------------------------------
  Binary instances
-------------------------------------------------------------------------------}

instance Binary Example
instance Binary Expansion
instance Binary GrammarPoint
instance Binary GrammarSentence
instance Binary Lesson
instance Binary LessonContent
instance Binary Level
instance Binary Sentence
instance Binary V3Id
instance Binary Vocabulary
instance Binary Word

instance Binary a => Binary (SearchResults a)

{-------------------------------------------------------------------------------
  PrettyVal instances
-------------------------------------------------------------------------------}

instance PrettyVal ReqGetLatestLessons
instance PrettyVal ReqGetLesson
instance PrettyVal ReqGetUserInfo
instance PrettyVal ReqLogin
instance PrettyVal ReqLogout
instance PrettyVal ReqSearchLessons
instance PrettyVal ReqSignature
instance PrettyVal RespGetUserInfo
instance PrettyVal RespLogin

instance PrettyVal AccessToken
instance PrettyVal Example
instance PrettyVal Expansion
instance PrettyVal GrammarPoint
instance PrettyVal GrammarSentence
instance PrettyVal Lesson
instance PrettyVal LessonContent
instance PrettyVal LessonContentType
instance PrettyVal Level
instance PrettyVal OK
instance PrettyVal Sentence
instance PrettyVal UserId
instance PrettyVal V3Id
instance PrettyVal Vocabulary
instance PrettyVal Word

instance PrettyVal a => PrettyVal (SearchResults a)
