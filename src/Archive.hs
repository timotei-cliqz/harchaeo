
module Archive where

import Control.Monad (forM)
import Data.Aeson
import Data.Either (either)
import Data.List (sortOn)
import Data.Map (Map, empty, singleton, unions)
import Data.Text (Text, unpack)
import System.Directory (listDirectory)
import System.FilePath.Posix
import System.Random (randomIO)
import Text.Printf (printf)
import System.Directory (removeDirectoryRecursive)
import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as B

import Types.Channel (Channel, name)
import Types.Message (Message, ts)
import Types.User    (User)


data Archive = Archive
    { channels  :: ![Channel]
    , users     :: ![User]
    , messages  :: !(Map Text [Message])
    } deriving (Show)


-- Load a single JSON from file
load :: FromJSON a => FilePath -> IO (Either String a)
load path = do
    content <- B.readFile path
    return (eitherDecode content)


loadMessagesForChannel :: FilePath -> Text -> IO (Map Text [Message])
loadMessagesForChannel baseDir channelName = do
    messages <- loadMessagesFromDirectory (baseDir </> unpack channelName)
    return $ singleton channelName (sortOn ts messages)
    where
        loadMessagesFromDirectory :: FilePath -> IO [Message]
        loadMessagesFromDirectory baseDir = do
            days <- listDirectory baseDir
            messages <- concat <$> (forM days $ \day -> loadMessagesFromFile (baseDir </> day))
            return messages
        loadMessagesFromFile :: FilePath -> IO [Message]
        loadMessagesFromFile dayPath = do
            result <- load dayPath :: IO (Either String [Message])
            case result of
                Left err -> do
                    putStrLn $ printf "Could not load %s: %s" dayPath err
                    return []
                Right messages -> return messages


loadMessages :: FilePath -> [Text] -> IO (Map Text [Message])
loadMessages baseDir channelNames = do
    maps <- forM channelNames $ \channel -> loadMessagesForChannel baseDir channel
    return $ unions maps


loadFromDirectory :: FilePath -> IO (Either String Archive)
loadFromDirectory path = do
    channels <- either (const []) id <$> load (path </> "channels.json")
    users <- either (const []) id <$> load (path </> "users.json")
    messages <- loadMessages path (map name channels)
    return . Right $ Archive channels users messages


loadFromArchive :: FilePath -> IO (Either String Archive)
loadFromArchive path = do
    -- Extract zip archive into a temporary directory
    content <- B.readFile path
    let zipArchive = Z.toArchive content
    randomNumber <- randomIO :: IO Int
    let destination = "/tmp/archive" ++ (show randomNumber)
    putStrLn $ "Extract archive into: " ++ destination
    Z.extractFilesFromArchive [Z.OptDestination destination] zipArchive
    -- Parse the content of the slack export into an Archive
    archive <- loadFromDirectory destination
    -- Clean-up temporary directory
    removeDirectoryRecursive destination
    return archive


loadArchive :: FilePath -> IO (Either String Archive)
loadArchive path
    | takeExtension path == ".zip" = loadFromArchive path
    | otherwise                    = loadFromDirectory path
