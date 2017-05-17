{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Message where

import GHC.Generics

import Elm          (ElmType)
import Data.Text    (Text)
import Data.Aeson   (FromJSON, ToJSON)


-- TODO: Deal with file_share subtype of messages
-- TODO: Deal with threads (thread_ts)

-- * Attachments

data Attachment = Attachment
    { title             :: !(Maybe Text)
    , image_url         :: !(Maybe Text)
    , from_url          :: !(Maybe Text)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)


-- * Reaction

data Reaction = Reaction
    { name      :: !Text
    , users     :: ![Text]
    , count     :: !Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

-- * Message

data Message = Message
    { user          :: !(Maybe Text)
    , reactions     :: !(Maybe [Reaction])
    , attachments   :: !(Maybe [Attachment])
    , text          :: !(Maybe Text)
    , ts            :: !Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)
