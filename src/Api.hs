{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where

import           Control.Monad.Trans.Except
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import Types.Channel


getChannels :: Handler [Channel]
getChannels = return [exampleChannel]

getChannelById :: String -> Handler Channel
getChannelById = \ case
  "officetalk" -> return exampleChannel
  _ -> throwE err404

exampleChannel :: Channel
exampleChannel = Channel "id" "name" "created" "creator" False False []


-- * api

type ChannelApi =
  "channel" :> Get '[JSON] [Channel] :<|>
  "channel" :> Capture "channel" String :> Get '[JSON] Channel

channelApi :: Proxy ChannelApi
channelApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve channelApi server

server :: Server ChannelApi
server =
  getChannels :<|>
  getChannelById

