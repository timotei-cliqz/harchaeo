{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where

import Prelude hiding (lookup)

import Control.Monad.Trans.Except
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Data.Text (Text)
import Data.Map (lookup)

import Archive          (Archive, getMessages, getChannels, getUsers)
import Types.Channel    (Channel)
import Types.User       (User)
import Types.Message    (Message)


-- * api

type ArchiveApi =
  "channel" :> Get '[JSON] [Channel] :<|>
  "channel" :> Capture "name" Text :> Get '[JSON] [Message] :<|>
  "user"    :> Get '[JSON] [User]

type FullApi = ArchiveApi :<|> Raw

fullApi :: Proxy FullApi
fullApi = Proxy

-- * app

run :: Archive -> Int -> IO ()
run archive port = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp
  where
    -- API

    mkApp :: IO Application
    mkApp = return $ serve fullApi server

    server :: Server FullApi
    server =
      (    getChannelsHandler
      :<|> getChannelByNameHandler
      :<|> getUsersHandler)
      :<|> serveDirectory "./"

    -- Handlers

    getChannelsHandler :: Handler [Channel]
    getChannelsHandler = return $ getChannels archive

    getChannelByNameHandler :: Text -> Handler [Message]
    getChannelByNameHandler name =
        case lookup name (getMessages archive) of
            Nothing -> throwE err404
            Just result -> return result

    getUsersHandler :: Handler [User]
    getUsersHandler = return (getUsers archive)
