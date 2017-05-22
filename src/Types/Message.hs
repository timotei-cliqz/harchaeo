{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Message where

import GHC.Generics

import Elm          (ElmType)
import Data.Text    (Text)
import Data.Aeson   (FromJSON, ToJSON)
import Types.User   (UserId)


-- TODO: Deal with file_share subtype of messages
-- TODO: Deal with threads (thread_ts)

-- * Attachments

type Title = Text
type Url = Text

data Attachment = Attachment
    { title             :: !(Maybe Title)
    , image_url         :: !(Maybe Url)
    , from_url          :: !(Maybe Url)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)


-- * Reaction

type Emoji = Text

data Reaction = Reaction
    { name      :: !Emoji
    , users     :: ![UserId]
    , count     :: !Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)

-- * Message


data Message = Message
    { user          :: !(Maybe UserId)
    , reactions     :: !(Maybe [Reaction])
    , attachments   :: !(Maybe [Attachment])
    , text          :: !(Maybe Text)
    , ts            :: !Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)
