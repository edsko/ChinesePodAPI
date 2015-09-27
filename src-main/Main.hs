module Main (main) where

import Data.Binary
import System.Directory
import Text.Show.Pretty
import Text.Printf (printf)
import Servant.ChinesePod.API
import Servant.ChinesePod.Client

import Options

exec :: ChinesePodAPI -> RespLogin -> Command -> EitherT ServantError IO ()
exec cpodAPI@ChinesePodAPI{..} respLogin = go
  where
    go :: Command -> EitherT ServantError IO ()
    go (CommandSearch opts) = do
      respSearchLessons <- cpodSearchLessons $ fromLogin respLogin opts
      liftIO $ putStrLn $ dumpStr respSearchLessons
    go (CommandLatest opts) = do
      respGetLatestLessons <- cpodGetLatestLessons $ fromLogin respLogin opts
      liftIO $ putStrLn $ dumpStr respGetLatestLessons
    go (CommandGetLesson opts) = do
      respGetLesson <- cpodGetLesson $ fromLogin respLogin opts
      liftIO $ putStrLn $ dumpStr respGetLesson
    go CommandDownloadIndex = do
      downloadIndex cpodAPI respLogin

-- | Download the full lesson index
downloadIndex :: ChinesePodAPI -> RespLogin -> EitherT ServantError IO ()
downloadIndex ChinesePodAPI{..} RespLogin{..} = do
    liftIO $ createDirectoryIfMissing True "./index"
    go 0
  where
    go :: Int -> EitherT ServantError IO ()
    go page = do
        pageDownloaded <- liftIO $ doesFileExist pageFile
        if pageDownloaded
          then do
            liftIO $ putStrLn $ "Skipping page " ++ show page
            go (page + 1)
          else do
            liftIO $ putStrLn $ "Downloading page " ++ show page
            respGetLatestLessons <- cpodGetLatestLessons ReqGetLatestLessons {
                reqGetLatestLessonsAccessToken = respLoginAccessToken
              , reqGetLatestLessonsUserId      = respLoginUserId
              , reqGetLatestLessonsPage        = Just page
              , reqGetLatestLessonsCount       = Just resultsPerPage
              , reqGetLatestLessonsLang        = Nothing
              , reqGetLatestLessonsLevelId     = Nothing
              }

            if null (searchResults respGetLatestLessons)
              then
                liftIO $ putStrLn $ "Done"
              else do
                liftIO $ encodeFile pageFile respGetLatestLessons
                go (page + 1)
      where
        pageFile = "./index/" ++ printf "%04d" page

    resultsPerPage :: Int
    resultsPerPage = 10

client :: ChinesePodAPI -> ReqLogin -> Command -> EitherT ServantError IO ()
client cpodAPI@ChinesePodAPI{..} reqLogin cmd = do
    respLogin <- cpodLogin reqLogin
    exec cpodAPI respLogin cmd
    OK <- cpodLogout $ fromLogin respLogin ()
    return ()

main :: IO ()
main = do
    Options{..} <- getOptions
    let cpodAPI = chinesePodAPI optionsBaseUrl
    mRes <- runEitherT $ client cpodAPI optionsReqLogin optionsCommand
    case mRes of
      Left  err -> print err
      Right ()  -> return ()
