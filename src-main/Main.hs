module Main (main) where

import Servant.ChinesePod.API
import Servant.ChinesePod.Client

import Options

client :: ChinesePodAPI -> ReqLogin -> EitherT ServantError IO ()
client ChinesePodAPI{..} reqLogin = do
    respLogin <- cpodLogin reqLogin
    liftIO $ print respLogin
    respLogout <- cpodLogout (fromLogin respLogin)
    liftIO $ print respLogout

main :: IO ()
main = do
    Options{..} <- getOptions
    let cpodAPI = chinesePodAPI optionsBaseUrl
    mRes <- runEitherT $ client cpodAPI optionsReqLogin
    case mRes of
      Left  err -> putStrLn $ "Error: " ++ show err
      Right ()  -> return ()
