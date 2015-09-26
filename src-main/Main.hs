module Main (main) where

import Servant.ChinesePod.API
import Servant.ChinesePod.Client

import Options

exec :: ChinesePodAPI -> RespLogin -> Command -> EitherT ServantError IO ()
exec ChinesePodAPI{..} respLogin = go
  where
    go :: Command -> EitherT ServantError IO ()
    go (CommandSearch opts) = do
      respSearchLessons <- cpodSearchLessons $ fromLogin respLogin opts
      liftIO $ print respSearchLessons
    go (CommandLatest opts) = do
      respGetLatestLessons <- cpodGetLatestLessons $ fromLogin respLogin opts
      liftIO $ print respGetLatestLessons
    go (CommandGetLesson opts) = do
      respGetLesson <- cpodGetLesson $ fromLogin respLogin opts
      liftIO $ print respGetLesson

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
