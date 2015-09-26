module Options (
    Options(..)
  , getOptions
  ) where

import Data.Typeable
import Options.Applicative
import Network.URI

import Servant.ChinesePod.API
import Servant.ChinesePod.Client

data Options = Options {
      optionsReqLogin :: ReqLogin
    , optionsBaseUrl  :: BaseUrl
    }
  deriving Show

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
readBaseUrl uriStr =
    case parseURI uriStr of
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
              ':':port   -> tryRead port
              _otherwise -> fail $ "Unexpected port " ++ show (uriPort auth)
            return (host, port)
          Nothing ->
            fail "No URI authority specified"
        return BaseUrl{..}
      Nothing ->
        fail $ "Could not parse URI"

tryRead :: forall a. (Typeable a, Read a) => String -> ReadM a
tryRead strA =
     case filter fullParse (readsPrec 0 strA) of
       [(a, _)]   -> return a
       _otherwise -> fail $ "Could not parse " ++ show strA ++ " "
                         ++ "as " ++ show (typeOf (undefined :: a))
   where
      fullParse :: (a, String) -> Bool
      fullParse = null . snd
