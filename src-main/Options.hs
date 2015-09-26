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
  deriving (Show)

data OptionsSearch = OptionsSearch {
      optionsSearchSearchLevel :: Maybe Level
    , optionsSearchNumResults  :: Maybe Int
    , optionsSearchPage        :: Maybe Int
    , optionsSearchSearch      :: String
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
          ])

parseOptionsSearch :: Parser OptionsSearch
parseOptionsSearch = OptionsSearch
    <$> (optional . option (str >>= autoFromText) $ mconcat [
            long "level"
          , help "Limit search to a specific level"
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
              ':':port   -> autoRead port
              _otherwise -> fail $ "Unexpected port " ++ show (uriPort auth)
            return (host, port)
          Nothing ->
            fail "No URI authority specified"
        return BaseUrl{..}
      Nothing ->
        fail $ "Could not parse URI"

autoFromText :: forall a. (FromText a, Typeable a) => String -> ReadM a
autoFromText strA =
    case fromText (T.pack strA) of
      Just a  -> return a
      Nothing -> fail $ "Could not parse " ++ show strA
                     ++ "as " ++ show (typeOf (undefined :: a))

autoRead :: forall a. (Typeable a, Read a) => String -> ReadM a
autoRead strA =
     case filter fullParse (readsPrec 0 strA) of
       [(a, _)]   -> return a
       _otherwise -> fail $ "Could not parse " ++ show strA ++ " "
                         ++ "as " ++ show (typeOf (undefined :: a))
   where
     fullParse :: (a, String) -> Bool
     fullParse = null . snd
