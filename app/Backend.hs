
module Main where

import System.Environment (getArgs)
import Lib (loadArchive, run)

main :: IO ()
main = do
    [path] <- getArgs
    json <- loadArchive path
    case json of
        Left err -> putStrLn err
        Right archive -> run archive
