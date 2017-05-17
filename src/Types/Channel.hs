{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Channel where

import GHC.Generics

import Elm          (ElmType)
import Data.Text    (Text)
import Data.Aeson   (FromJSON, ToJSON)


-- * Channel

data Channel = Channel
    { id            :: !Text
    , name          :: !Text
    , created       :: !Text
    , creator       :: !Text
    , is_archived   :: !Bool
    , is_general    :: !Bool
    , members       :: ![Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)
