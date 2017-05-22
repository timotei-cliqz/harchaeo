{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}


module Main where

import Options.Generic
import Data.Maybe (fromMaybe)
import Lib (loadArchive, run)


-- Argument parser
data Harchaeo = Harchaeo
    { path :: String
    , port :: Maybe Int
    } deriving (Generic, Show)

instance ParseRecord Harchaeo


main :: IO ()
main = do
    -- Parse arguments
    (args :: Harchaeo) <- getRecord "Harchaeo - Slack export viewer"
    let p = fromMaybe 8000 (port args)

    -- Serve archive content
    json <- loadArchive $ path args
    case json of
        Just archive -> run archive p
        Nothing -> return ()
