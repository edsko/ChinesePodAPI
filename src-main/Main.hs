{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary
import System.Directory
import Servant.Client (ServantError(..))
import Text.Show.Pretty
import Text.Printf (printf)
import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as BS.L

import Servant.ChinesePod.API
import Servant.ChinesePod.Client
import qualified Servant.ChinesePod.Vocab as Vocab

import Options

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

-- | Export all vocabulary to a single file
exportVocab :: IO ()
exportVocab = do
    lessonFiles <- filter (not . hidden) <$> getDirectoryContents "./content"
    lessons     <- mapM decodeFile $ map ("./content/" ++) lessonFiles
    encodeFile "./vocab" $ Vocab.extractVocab lessons
  where
    hidden :: FilePath -> Bool
    hidden ('.':_) = True
    hidden _       = False

exec :: Command -> IO ()
exec (CommandSearch optsCPod optsCmd) =
    withCPod optsCPod $ \ChinesePodAPI{..} respLogin -> do
      respSearchLessons <- cpodSearchLessons $ fromLogin respLogin optsCmd
      liftIO $ putStrLn $ dumpStr respSearchLessons
exec (CommandLatest optsCPod optsCmd) =
    withCPod optsCPod $ \ChinesePodAPI{..} respLogin -> do
      respGetLatestLessons <- cpodGetLatestLessons $ fromLogin respLogin optsCmd
      liftIO $ putStrLn $ dumpStr respGetLatestLessons
exec (CommandGetLesson optsCPod optsCmd) =
    withCPod optsCPod $ \ChinesePodAPI{..} respLogin -> do
      respGetLesson <- cpodGetLesson $ fromLogin respLogin optsCmd
      liftIO $ putStrLn $ dumpStr respGetLesson
exec (CommandDownloadIndex optsCPod) =
    withCPod optsCPod $ downloadIndex
exec (CommandDownloadContent optsCPod) =
    withCPod optsCPod $ downloadContent
exec CommandExportVocab =
    exportVocab

withCPod :: OptionsCPod
         -> (ChinesePodAPI -> RespLogin -> EitherT ServantError IO ())
         -> IO ()
withCPod OptionsCPod{..} handler = do
    mRes <- runEitherT runHandler
    case mRes of
      Left  err -> logServantError err
      Right ()  -> return ()
  where
    runHandler :: EitherT ServantError IO ()
    runHandler = do
        respLogin <- cpodLogin optionsReqLogin
        handler cpodAPI respLogin
        OK <- cpodLogout $ fromLogin respLogin ()
        return ()

    cpodAPI@ChinesePodAPI{..} = chinesePodAPI optionsBaseUrl

logServantError :: ServantError -> IO ()
logServantError err = do
    let (err', mBody) = body err
    case mBody of
      Nothing -> return ()
      Just bs -> BS.L.writeFile "responseBody.servant" bs
    print err'
  where
    body :: ServantError -> (ServantError, Maybe BS.L.ByteString)
    body FailureResponse{..} =
      (FailureResponse{responseBody = omitted, ..}, Just responseBody)
    body DecodeFailure{..} =
      (DecodeFailure{responseBody = omitted, ..}, Just responseBody)
    body UnsupportedContentType{..} =
      (UnsupportedContentType{responseBody = omitted, ..}, Just responseBody)
    body ConnectionError{..} =
      (ConnectionError{..}, Nothing)
    body InvalidContentTypeHeader{..} =
      (InvalidContentTypeHeader{responseBody = omitted, ..}, Just responseBody)

    omitted :: BS.L.ByteString
    omitted = "<<written to responseBody.servant>>"

main :: IO ()
main = do
    Options{..} <- getOptions
    exec optionsCommand
