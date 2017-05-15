

module Archive where


import Data.Aeson
import Types


data Archive = Archive
    {

    } deriving (Show)


loadFromArchive :: Bytestring -> IO Archive
loadFromArchive _ = return
