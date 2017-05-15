{-# LANGUAGE DeriveGeneric #-}

module Types.Channel where


import Data.Text (Text)
import Data.Aeson
import GHC.Generics


-- * Channel

data Channel = Channel
    { id            :: !Text
    , name          :: !Text
    , created       :: !Text
    , creator       :: !Text
    , is_archived   :: !Bool
    , is_general    :: !Bool
    , members       :: ![Text]
    } deriving (Eq, Show, Generic)

instance ToJSON Channel
instance FromJSON Channel
