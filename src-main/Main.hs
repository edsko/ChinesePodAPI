{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.Binary
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import System.Directory
import Text.Printf (printf)
import Text.Show.Pretty
import qualified Data.Map             as Map
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Text            as T

import Servant.ChinesePod.API
import Servant.ChinesePod.Client
import qualified Servant.ChinesePod.Vocab.V2 as Vocab

import Options

-- | Download the full lesson index
downloadIndex :: ChinesePodAPI -> RespLogin -> ClientM ()
downloadIndex ChinesePodAPI{..} RespLogin{..} = do
    liftIO $ createDirectoryIfMissing True "./index"
    go 0
  where
    go :: Int -> ClientM ()
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
downloadContent :: ChinesePodAPI -> RespLogin -> ClientM ()
downloadContent ChinesePodAPI{..} RespLogin{..} = do
    liftIO $ createDirectoryIfMissing True "./content"
    goPage 0
  where
    goPage :: Int -> ClientM ()
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

    goLesson :: Lesson -> ClientM ()
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
    lessonFiles  <- filter (not . hidden) <$> getDirectoryContents "./content"
    lessons      <- mapM decodeFile $ map ("./content/" ++) lessonFiles
    let (skipped, vocab) = Vocab.extractVocab lessons
    encodeFile "./vocab" vocab
    forM_ skipped $ \Vocab.Skipped{..} -> putStrLn $ concat [
        "Skipped " ++ v3IdString skippedV3Id
      , " (" ++ skippedTitle ++ "): "
      , T.unpack skippedReason
      ]
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
         -> (ChinesePodAPI -> RespLogin -> ClientM ())
         -> IO ()
withCPod OptionsCPod{..} handler = do
    mgr  <- newManager tlsManagerSettings
    mRes <- runClientM runHandler (ClientEnv mgr optionsBaseUrl)
    case mRes of
      Left  err -> logServantError err
      Right ()  -> return ()
  where
    runHandler :: ClientM ()
    runHandler = do
        respLogin <- cpodLogin optionsReqLogin
        handler cpodAPI respLogin
        OK <- cpodLogout $ fromLogin respLogin ()
        return ()

    cpodAPI@ChinesePodAPI{..} = chinesePodAPI

logServantError :: ServantError -> IO ()
logServantError err = do
    let (err', mBody) = body err
    case mBody of
      Nothing -> return ()
      Just bs -> BS.L.writeFile "responseBody.servant" bs
    print err'
  where
    body :: ServantError -> (ServantError, Maybe BS.L.ByteString)
    body (FailureResponse            r) = ( FailureResponse            r { responseBody = omitted } , Just (responseBody r) )
    body (DecodeFailure txt          r) = ( DecodeFailure txt          r { responseBody = omitted } , Just (responseBody r) )
    body (UnsupportedContentType typ r) = ( UnsupportedContentType typ r { responseBody = omitted } , Just (responseBody r) )
    body (InvalidContentTypeHeader   r) = ( InvalidContentTypeHeader   r { responseBody = omitted } , Just (responseBody r) )
    body (ConnectionError e           ) = ( ConnectionError e                                       , Nothing               )

    omitted :: BS.L.ByteString
    omitted = "<<written to responseBody.servant>>"

main :: IO ()
main = do
    Options{..} <- getOptions
    exec optionsCommand
