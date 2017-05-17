
module Main where

import System.Environment (getArgs)
import Lib (loadFromDirectory, run)

main :: IO ()
main = do
    [path] <- getArgs
    json <- loadFromDirectory path
    case json of
        Left err -> putStrLn err
        Right archive -> run archive
