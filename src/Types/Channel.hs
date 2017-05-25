{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Channel where

import GHC.Generics

import Elm          (ElmType)
import Data.Text    (Text)
import Data.Aeson   (FromJSON, ToJSON)
import Types.User   (UserId)


-- * Channel

type ChannelId = Text
type ChannelName = Text

data Channel = Channel
    { id            :: !ChannelId
    , name          :: !ChannelName
    , creator       :: !UserId
    , created       :: !Text
    , is_archived   :: !Bool
    , is_general    :: !Bool
    , members       :: ![Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)
