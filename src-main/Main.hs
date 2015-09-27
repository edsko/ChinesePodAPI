module Main (main) where

import Data.Binary
import System.Directory
import Text.Show.Pretty
import Text.Printf (printf)
import Servant.ChinesePod.API
import Servant.ChinesePod.Client
import qualified Data.Map as Map

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
    go CommandDownloadIndex   = downloadIndex   cpodAPI respLogin
    go CommandDownloadContent = downloadContent cpodAPI respLogin

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
                liftIO $ putStrLn "Done"
              else do
                liftIO $ encodeFile pageFile respGetLatestLessons
                go (page + 1)
      where
        pageFile = "./index/" ++ printf "%04d" page

    resultsPerPage :: Int
    resultsPerPage = 10

-- | Download lesson content
downloadContent :: ChinesePodAPI -> RespLogin -> EitherT ServantError IO ()
downloadContent ChinesePodAPI{..} RespLogin{..} = do
    liftIO $ createDirectoryIfMissing True "./content"
    goPage 0
  where
    goPage :: Int -> EitherT ServantError IO ()
    goPage pageNum = do
        pageExists <- liftIO $ doesFileExist pageFile
        if not pageExists
          then liftIO $ putStrLn "Done"
          else do
            page <- liftIO $ decodeFile pageFile
            mapM_ (goLesson . snd) (Map.toList (searchResults page))
            goPage (pageNum + 1)
      where
        pageFile = "./index/" ++ printf "%04d" pageNum

    goLesson :: Lesson -> EitherT ServantError IO ()
    goLesson Lesson{lessonV3Id = V3Id lessonId} = do
       lessonExists <- liftIO $ doesFileExist lessonFile
       if lessonExists
         then do
           liftIO $ putStrLn $ "Skipping lesson " ++ lessonId
         else do
           liftIO $ putStrLn $ "Downloading lesson " ++ lessonId
           content <- cpodGetLesson ReqGetLesson {
               reqGetLessonAccessToken = respLoginAccessToken
             , reqGetLessonUserId      = respLoginUserId
             , reqGetLessonV3Id        = V3Id lessonId
             , reqGetLessonType        = Nothing
             }
           liftIO $ encodeFile lessonFile content
      where
        lessonFile = "./content/" ++ lessonId

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
