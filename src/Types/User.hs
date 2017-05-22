{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.User where

import GHC.Generics

import Elm          (ElmType)
import Data.Text    (Text)
import Data.Aeson   (FromJSON, ToJSON)


-- * Profile

data Profile = Profile
    { avatar_hash           :: !(Maybe Text)
    , image_24              :: !(Maybe Text)
    , image_32              :: !(Maybe Text)
    , image_48              :: !(Maybe Text)
    , image_72              :: !(Maybe Text)
    , image_192             :: !(Maybe Text)
    , image_512             :: !(Maybe Text)
    , image_1024            :: !(Maybe Text)
    , image_original        :: !(Maybe Text)
    , real_name             :: !(Maybe Text)
    , email                 :: !(Maybe Text)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)


-- * User

type UserId = Text
type UserName = Text

data User = User
    { id                    :: !UserId
    , name                  :: !UserName
    , color                 :: !(Maybe Text)
    , deleted               :: !(Maybe Bool)
    , is_admin              :: !(Maybe Bool)
    , is_owner              :: !(Maybe Bool)
    , is_primary_owner      :: !(Maybe Bool)
    , is_restricted         :: !(Maybe Bool)
    , is_ultra_restricted   :: !(Maybe Bool)
    , is_bot                :: !(Maybe Bool)
    , profile               :: !(Maybe Profile)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ElmType)
