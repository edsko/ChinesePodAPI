module Options (
    Command(..)
  , Options(..)
  , OptionsSearch(..)
  , getOptions
  ) where

import Data.Typeable
import Options.Applicative
import Network.URI
import qualified Data.Text as T

import Servant.API
import Servant.ChinesePod.API
import Servant.ChinesePod.Client

data Options = Options {
      optionsReqLogin :: ReqLogin
    , optionsBaseUrl  :: BaseUrl
    , optionsCommand  :: Command
    }
  deriving (Show)

data Command =
    CommandSearch OptionsSearch
  | CommandLatest OptionsLatest
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

{-------------------------------------------------------------------------------
  Constructing requests
-------------------------------------------------------------------------------}

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
    <$> parseReqLogin
    <*> parseBaseUrl
    <*> (subparser $ mconcat [
            command "search" $
              info (helper <*> (CommandSearch <$> parseOptionsSearch))
                   (progDesc "Search for lessons")
          , command "latest" $
              info (helper <*> (CommandLatest <$> parseOptionsLatest))
                   (progDesc "Get the list of the latest lessons")
          ])

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
parseBaseUrl = option (str >>= readBaseUrl) $ mconcat [
      long "base-url"
    , value (BaseUrl Http "chinesepod.com" 80)
    , help "ChinesePod host (defaults to http://chinesepod.com:80)"
    , metavar "URI"
    ]

readBaseUrl :: String -> ReadM BaseUrl
readBaseUrl strURI =
    case parseURI strURI of
      Just uri -> do
        baseUrlScheme <- case uriScheme uri of
          "http:"    -> return Http
          "https:"   -> return Https
          _otherwise -> fail $ "Unsupported URI scheme " ++ show (uriScheme uri)
        (baseUrlHost, baseUrlPort) <- case uriAuthority uri of
          Just auth -> do
            let host = uriRegName auth
            port <- case uriPort auth of
              ""         -> return 80
              ':':port   -> autoFromRead port
              _otherwise -> fail $ "Unexpected port " ++ show (uriPort auth)
            return (host, port)
          Nothing ->
            fail "No URI authority specified"
        return BaseUrl{..}
      Nothing ->
        fail $ "Could not parse URI"

autoFromRead :: forall a. (Read a, Typeable a) => String -> ReadM a
autoFromRead strA =
    case tryRead strA of
      Just a  -> return a
      Nothing -> parseFailure (T.pack strA)

autoFromText :: forall a. (FromText a, Typeable a) => String -> ReadM a
autoFromText strA =
    case fromText (T.pack strA) of
      Just a  -> return a
      Nothing -> parseFailure (T.pack strA)
