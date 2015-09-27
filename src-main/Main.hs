module Main (main) where

import Text.Show.Pretty
import Servant.ChinesePod.API
import Servant.ChinesePod.Client

import Options

exec :: ChinesePodAPI -> RespLogin -> Command -> EitherT ServantError IO ()
exec ChinesePodAPI{..} respLogin = go
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

client :: ChinesePodAPI -> ReqLogin -> Command -> EitherT ServantError IO ()
client cpodAPI@ChinesePodAPI{..} reqLogin cmd = do
    respLogin <- cpodLogin reqLogin
    liftIO $ putStrLn $ dumpStr respLogin
    exec cpodAPI respLogin cmd
    OK <- cpodLogout $ fromLogin respLogin ()
    liftIO $ putStrLn $ dumpStr OK
    return ()

main :: IO ()
main = do
    Options{..} <- getOptions
    let cpodAPI = chinesePodAPI optionsBaseUrl
    mRes <- runEitherT $ client cpodAPI optionsReqLogin optionsCommand
    case mRes of
      Left  err -> print err
      Right ()  -> return ()
