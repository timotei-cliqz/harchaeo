{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Main where

import Data.Proxy (Proxy (Proxy))
import Elm
    ( Spec (Spec)
    , specsToDir
    , toElmDecoderSource
    , toElmEncoderSource
    , toElmTypeSource
    )
import Servant.Elm
    ( ElmOptions (..)
    , defElmImports
    , defElmOptions
    , generateElmForAPIWith
    , UrlPrefix (Static)
    )

import Api           (ChannelApi)
import Types.Message (Message)
import Types.Channel (Channel)
import Types.User    (User)


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8000/api" }

spec :: Spec
spec =
  Spec ["Generated", "Api"]
    (defElmImports
    : toElmTypeSource    (Proxy :: Proxy Message)
    : toElmDecoderSource (Proxy :: Proxy Message)
    : toElmEncoderSource (Proxy :: Proxy Message)
    : toElmTypeSource    (Proxy :: Proxy Channel)
    : toElmDecoderSource (Proxy :: Proxy Channel)
    : toElmEncoderSource (Proxy :: Proxy Channel)
    : toElmTypeSource    (Proxy :: Proxy User)
    : toElmDecoderSource (Proxy :: Proxy User)
    : toElmEncoderSource (Proxy :: Proxy User)
    : generateElmForAPIWith elmOpts (Proxy :: Proxy ChannelApi))

main :: IO ()
main = specsToDir [spec] "frontend/src"
