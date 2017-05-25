
module Archive where


import qualified Codec.Archive.Zip      as Z
import           Control.Error          (ExceptT, catchE, runExceptT, throwE)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as B
import           Data.List              (sortOn)
import           Data.Map               (Map, fromList)
import           Data.Text              (unpack)
import           System.Directory       (listDirectory,
                                         removeDirectoryRecursive)
import           System.FilePath.Posix
import           System.Random          (randomIO)
import           Text.Printf            (printf)

import           Types.Channel          (Channel, ChannelName, name)
import           Types.Message          (Message, ts)
import           Types.User             (User)


type ErrorMsg = String
type Loader a = ExceptT ErrorMsg IO a

data Archive = Archive
    { getChannels :: ![Channel]
    , getUsers    :: ![User]
    , getMessages :: !(Map ChannelName [Message])
    } deriving (Show)


-- | Load a single JSON from file
load :: FromJSON a => FilePath -> Loader a
load path = do
    content <- liftIO (B.readFile path)
    case eitherDecode content of
        Left err      -> throwE $ printf "Could not load %s: %s" path err
        Right decoded -> return decoded


-- | Load all messages for a given channel (accumulate all days)
loadMessagesForChannel :: FilePath -> ChannelName -> Loader [Message]
loadMessagesForChannel baseDir channelName = do
    let channelDirectory = baseDir </> unpack channelName
    days <- liftIO (listDirectory channelDirectory)
    concat <$> forM days (\day -> load (channelDirectory </> day))


-- | Load messages of all channels
loadMessages :: FilePath -> [ChannelName] -> Loader (Map ChannelName [Message])
loadMessages baseDir channelNames =
    fromList <$> forM channelNames (\channelName -> do
      messages <- loadMessagesForChannel baseDir channelName
      return (channelName, sortOn ts messages))


-- | Load archive from directory containing unzipped Slack export
loadFromDirectory :: FilePath -> Loader Archive
loadFromDirectory path = do
    channels <- load (path </> "channels.json")
    users    <- load (path </> "users.json")
    messages <- loadMessages path (map name channels)
    return $ Archive channels users messages


-- | Load archive from zip file directly (extracts content in a temp directory)
loadFromArchive :: FilePath -> Loader Archive
loadFromArchive path = do
  -- Create temporary directory
  randomNumber <- liftIO (randomIO :: IO Int)
  let tmpDir = "/tmp/archive" ++ show randomNumber
  liftIO (putStrLn $ "Extract archive into: " ++ tmpDir)

  -- Extract zip archive into a temporary directory
  content <- liftIO (B.readFile path)
  let zipArchive = Z.toArchive content
  liftIO (Z.extractFilesFromArchive
           [Z.OptDestination tmpDir]
           zipArchive)

  -- Make sure to clean-up temp directory on exception
  archive <- catchE (loadFromDirectory tmpDir) $ \err -> do
    cleanup tmpDir
    throwE err

  -- Clean-up temp directory
  cleanup tmpDir
  return archive

  where
    cleanup tmpDir = do
      liftIO (putStrLn $ "Clean-up temp directory " ++ tmpDir)
      liftIO (removeDirectoryRecursive tmpDir)


-- | Load archive from either the archive or a directory
loadArchive :: FilePath -> IO (Either ErrorMsg Archive)
loadArchive = runExceptT . go
  where go path
          | takeExtension path == ".zip" = loadFromArchive path
          | otherwise                    = loadFromDirectory path
