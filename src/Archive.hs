
module Archive where

import Control.Monad (forM)
import Data.Aeson
import Data.List (sortOn)
import Data.Map (Map, singleton, unions)
import Data.Text (Text, unpack)
import System.FilePath.Posix
import System.Random (randomIO)
import Text.Printf (printf)
import System.Directory (removeDirectoryRecursive, listDirectory)
import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as B

import Types.Channel (Channel, name)
import Types.Message (Message, ts)
import Types.User    (User)


data Archive = Archive
    { getChannels :: ![Channel]
    , getUsers    :: ![User]
    , getMessages :: !(Map Text [Message])
    } deriving (Show)


logErrors :: [String] -> IO ()
logErrors = mapM_ putStrLn


-- Load a single JSON from file
load :: FromJSON a => FilePath -> IO (Maybe a)
load path = do
    content <- B.readFile path
    case eitherDecode content of
        Left err -> do
            putStrLn $ printf "Could not load %s: %s" path err
            return Nothing
        Right decoded -> return decoded


loadMessagesForChannel :: FilePath -> Text -> IO (Map Text [Message])
loadMessagesForChannel baseDir channelName = do
    messages <- loadMessagesFromDirectory (baseDir </> unpack channelName)
    return $ singleton channelName (sortOn ts messages)
    where
        loadMessagesFromDirectory channelDir = do
            days <- listDirectory channelDir
            concat <$> forM days (\day -> loadMessagesFromFile (channelDir </> day))
        loadMessagesFromFile dayPath = do
            result <- load dayPath
            case result of
                Nothing -> return []
                Just messages -> return messages


loadMessages :: FilePath -> [Text] -> IO (Map Text [Message])
loadMessages baseDir channelNames =
    unions <$> forM channelNames (\channel -> loadMessagesForChannel baseDir channel)


loadFromDirectory :: FilePath -> IO (Maybe Archive)
loadFromDirectory path = do
    resultChannels <- load (path </> "channels.json")
    resultUsers    <- load (path </> "users.json")
    case (resultChannels, resultUsers) of
        (Just channels, Just users) -> do
            messages <- loadMessages path (map name channels)
            return . Just $ Archive channels users messages
        _ -> return Nothing


loadFromArchive :: FilePath -> IO (Maybe Archive)
loadFromArchive path = withExtractedZip $ \dir -> loadFromDirectory dir
    where
        withTempDir cb = do
            -- Create temporary directory
            randomNumber <- randomIO :: IO Int
            let tmpDir = "/tmp/archive" ++ show randomNumber
            putStrLn $ "Extract archive into: " ++ tmpDir
            result <- cb tmpDir
            -- Clean-up temporary directory
            removeDirectoryRecursive tmpDir
            return result

        withExtractedZip cb =
            withTempDir $ \tmpDir -> do
                -- Extract zip archive into a temporary directory
                content <- B.readFile path
                let zipArchive = Z.toArchive content
                Z.extractFilesFromArchive [Z.OptDestination tmpDir] zipArchive
                cb tmpDir


loadArchive :: FilePath -> IO (Maybe Archive)
loadArchive path
    | takeExtension path == ".zip" = loadFromArchive path
    | otherwise                    = loadFromDirectory path
