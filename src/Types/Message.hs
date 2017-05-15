{-# LANGUAGE DeriveGeneric #-}

module Types.Message where


import Data.Text (Text)
import Data.Aeson
import GHC.Generics

-- TODO: Deal with file_share subtype of messages
-- TODO: Deal with threads (thread_ts)

-- * Attachments

data Attachment = Attachment
    { service_name      :: !Text
    , title             :: !Text
    , from_url          :: !Text
    , image_url         :: !Text
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
    { user          :: !Text
    , text          :: !Text
    , ts            :: !Text
    , reactions     :: ![Reaction]
    , attachments   :: ![Attachment]
    } deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message
