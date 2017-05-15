
module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    [path] <- getArgs
    json <- loadFromDirectory path
    case json of
        Left error -> putStrLn error
        Right archive -> run archive
