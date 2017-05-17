{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where

import Prelude hiding (lookup)

import Control.Monad.Trans.Except
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Utils.StaticFiles
import System.IO

import Data.Text (Text)
import Data.Map (lookup)

import Archive          (Archive, messages, channels)
import Types.Channel    (Channel)
import Types.User       (User)
import Types.Message    (Message)


-- * api

type ChannelApi =
  "channel" :> Get '[JSON] [Channel] :<|>
  "channel" :> Capture "name" Text :> Get '[JSON] [Message]

type FullApi = ChannelApi :<|> Raw

fullApi :: Proxy FullApi
fullApi = Proxy

-- * app

run :: Archive -> IO ()
run archive = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp
  where
    mkApp :: IO Application
    mkApp = return $ serve fullApi server

    server :: Server FullApi
    server =
      (getChannels :<|>
      getChannelByName) :<|>
      serveDirectory "./"

    getChannels :: Handler [Channel]
    getChannels = return . channels $ archive

    getChannelByName :: Text -> Handler [Message]
    getChannelByName name =
        case lookup name (messages archive) of
            Nothing -> throwE err404
            Just result -> return result
