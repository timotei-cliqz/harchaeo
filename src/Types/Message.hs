{-# LANGUAGE DeriveGeneric #-}

module Types.Message where


import Data.Text (Text)
import Data.Aeson
import GHC.Generics

-- TODO: Deal with file_share subtype of messages
-- TODO: Deal with threads (thread_ts)

-- * Attachments

data Attachment = Attachment
    { title             :: !(Maybe Text)
    , image_url         :: !(Maybe Text)
    , from_url          :: !(Maybe Text)
    } deriving (Eq, Show, Generic)

instance ToJSON Attachment
instance FromJSON Attachment


-- * Reaction

data Reaction = Reaction
    { name      :: !Text
    , users     :: ![Text]
    , count     :: !Int
    } deriving (Eq, Show, Generic)

instance ToJSON Reaction
instance FromJSON Reaction

-- * Message

data Message = Message
    { user          :: !(Maybe Text)
    , reactions     :: !(Maybe [Reaction])
    , attachments   :: !(Maybe [Attachment])
    , text          :: !(Maybe Text)
    , ts            :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message
