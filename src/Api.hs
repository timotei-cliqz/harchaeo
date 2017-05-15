{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where

import Prelude hiding (lookup)

import           Control.Monad.Trans.Except
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import Data.Text (Text)
import Data.Map (lookup)

import Archive
import Types.Channel
import Types.User
import Types.Message (Message)


-- * api

type ChannelApi =
  "channel" :> Get '[JSON] [Channel] :<|>
  "channel" :> Capture "channel" Text :> Get '[JSON] [Message]

channelApi :: Proxy ChannelApi
channelApi = Proxy

-- * app

run :: Archive -> IO ()
run archive = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp
  where
    mkApp :: IO Application
    mkApp = return $ serve channelApi server

    server :: Server ChannelApi
    server =
      getChannels :<|>
      getChannelById

    getChannels :: Handler [Channel]
    getChannels = return . channels $ archive

    getChannelById :: Text -> Handler [Message]
    getChannelById name =
        case lookup name (messages archive) of
            Nothing -> throwE err404
            Just result -> return result
