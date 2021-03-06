{-# OPTIONS_GHC -fno-warn-orphans #-}
module Options (
    Command(..)
  , Options(..)
  , OptionsCPod(..)
  , OptionsSearch(..)
  , OptionsGetLesson(..)
  , getOptions
  , FromLogin(..)
  ) where

import Control.Monad.Catch
import Data.Typeable
import Options.Applicative
import qualified Data.Text as T

import Servant.API
import Servant.ChinesePod.API
import Servant.ChinesePod.Client
import qualified Servant.Client as Servant

data Options = Options {
      optionsCommand  :: Command
    }
  deriving (Show)

data OptionsCPod = OptionsCPod {
      optionsReqLogin :: ReqLogin
    , optionsBaseUrl  :: BaseUrl
    }
  deriving (Show)

data Command =
    CommandSearch          OptionsCPod OptionsSearch
  | CommandLatest          OptionsCPod OptionsLatest
  | CommandGetLesson       OptionsCPod OptionsGetLesson
  | CommandDownloadIndex   OptionsCPod
  | CommandDownloadContent OptionsCPod
  | CommandExportVocab
  deriving (Show)

data OptionsSearch = OptionsSearch {
      optionsSearchSearchLevel :: Maybe Level
    , optionsSearchNumResults  :: Maybe Int
    , optionsSearchPage        :: Maybe Int
    , optionsSearchSearch      :: String
    }
  deriving (Show)

data OptionsLatest = OptionsLatest {
      optionsLatestPage    :: Maybe Int
    , optionsLatestCount   :: Maybe Int
    , optionsLatestLang    :: Maybe String
    , optionsLatestLevelId :: Maybe Level
    }
  deriving (Show)

data OptionsGetLesson = OptionsGetLesson {
      optionsGetLessonV3Id :: V3Id
    , optionsGetLessonType :: Maybe LessonContentType
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Constructing requests
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

instance FromLogin ReqSearchLessons OptionsSearch where
    fromLogin RespLogin{..} OptionsSearch{..} = ReqSearchLessons {
        reqSearchLessonsAccessToken = respLoginAccessToken
      , reqSearchLessonsUserId      = respLoginUserId
      , reqSearchLessonsSearch      = optionsSearchSearch
      , reqSearchLessonsSearchLevel = optionsSearchSearchLevel
      , reqSearchLessonsNumResults  = optionsSearchNumResults
      , reqSearchLessonsPage        = optionsSearchPage
      }

instance FromLogin ReqGetLatestLessons OptionsLatest where
    fromLogin RespLogin{..} OptionsLatest{..} = ReqGetLatestLessons {
        reqGetLatestLessonsAccessToken = respLoginAccessToken
      , reqGetLatestLessonsUserId      = respLoginUserId
      , reqGetLatestLessonsPage        = optionsLatestPage
      , reqGetLatestLessonsCount       = optionsLatestCount
      , reqGetLatestLessonsLang        = optionsLatestLang
      , reqGetLatestLessonsLevelId     = optionsLatestLevelId
      }

instance FromLogin ReqGetLesson OptionsGetLesson where
    fromLogin RespLogin{..} OptionsGetLesson{..} = ReqGetLesson {
        reqGetLessonAccessToken = respLoginAccessToken
      , reqGetLessonUserId      = respLoginUserId
      , reqGetLessonV3Id        = optionsGetLessonV3Id
      , reqGetLessonType        = optionsGetLessonType
      }

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> parseOptions) $ mconcat [
        fullDesc
      , progDesc "Example client to query the ChinesePod API"
      ]

parseOptions :: Parser Options
parseOptions = Options
    <$> (subparser $ mconcat [
            command "search" $
              info (helper <*> (CommandSearch <$> parseOptionsCPod <*> parseOptionsSearch))
                   (progDesc "Search for lessons")
          , command "latest" $
              info (helper <*> (CommandLatest <$> parseOptionsCPod <*> parseOptionsLatest))
                   (progDesc "Get the list of the latest lessons")
          , command "get-lesson" $
              info (helper <*> (CommandGetLesson <$> parseOptionsCPod <*> parseOptionsGetLesson))
                   (progDesc "Return the contents of a particular lesson")
          , command "download-index" $
              info (helper <*> (CommandDownloadIndex <$> parseOptionsCPod))
                   (progDesc "Download the full lesson index")
          , command "download-content" $
              info (helper <*> (CommandDownloadContent <$> parseOptionsCPod))
                   (progDesc "Download lesson content (download index first)")
          , command "export-vocab" $
              info (helper <*> (pure CommandExportVocab))
                   (progDesc "Export vocabulary (download content first)")
          ])

parseOptionsCPod :: Parser OptionsCPod
parseOptionsCPod = OptionsCPod
    <$> parseReqLogin
    <*> parseBaseUrl

parseOptionsSearch :: Parser OptionsSearch
parseOptionsSearch = OptionsSearch
    <$> (option (str >>= fmap strOrInt . autoFromText) $ mconcat [
            long "level"
          , help "Limit search to a specific level"
          , value Nothing
          ])
    <*> (optional . option auto $ mconcat [
            long "num-results"
          , help "Number of results"
          ])
    <*> (optional . option auto $ mconcat [
            long "page"
          , help "Page number"
          ])
    <*> argument str (metavar "KEYWORD")

parseOptionsLatest :: Parser OptionsLatest
parseOptionsLatest = OptionsLatest
    <$> (optional . option auto $ mconcat [
            long "page"
          , help "Page number"
          ])
    <*> (optional . option auto $ mconcat [
            long "count"
          , help "Number of lessons per page"
          ])
    <*> (optional . strOption $ mconcat [
            long "lang"
          , help "Language local settings"
          ])
    <*> (option (str >>= fmap strOrInt . autoFromText) $ mconcat [
            long "level"
          , help "Level"
          , value Nothing
          ])

parseOptionsGetLesson :: Parser OptionsGetLesson
parseOptionsGetLesson = OptionsGetLesson
    <$> (option (str >>= autoFromText) $ mconcat [
            long "v3id"
          , help "The unique ID of the lesson"
          ])
    <*> (optional . option (str >>= autoFromText) $ mconcat [
            long "type"
          , help "Which type of content to return (all, exercise, vocabulary, dialogue, grammar)"
          ])

parseReqLogin :: Parser ReqLogin
parseReqLogin = ReqLogin
    <$> (strOption $ mconcat [
            long "client-id"
          , metavar "ID"
          , help "ChinesePod Client API ID"
          ])
    <*> (strOption $ mconcat [
            long "user-email"
          , metavar "EMAIL"
          , help "ChinesePod user email address"
          ])
    <*> parseReqSignature

parseReqSignature :: Parser ReqSignature
parseReqSignature = ReqSignature
    <$> (strOption $ mconcat [
            long "client-secret"
          , metavar "SECRET"
          , help "ChinesePod Client API secret"
          ])
    <*> (strOption $ mconcat [
            long "user-password"
          , metavar "PASS"
          , help "ChinesePod user password"
          ])

parseBaseUrl :: Parser BaseUrl
parseBaseUrl = option (str >>= Servant.parseBaseUrl) $ mconcat [
      long "base-url"
    , value (BaseUrl Http "chinesepod.com" 80 "/")
    , help "ChinesePod host (defaults to http://chinesepod.com:80)"
    , metavar "URI"
    ]

autoFromText :: forall a. (FromHttpApiData a, Typeable a) => String -> ReadM a
autoFromText strA =
    case parseQueryParam (T.pack strA) of
      Right a   -> return a
      Left  err -> parseFailure err

{-------------------------------------------------------------------------------
  Auxiliary: orphans
-------------------------------------------------------------------------------}

instance MonadThrow ReadM where
  throwM = fail . show
