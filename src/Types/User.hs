{-# LANGUAGE DeriveGeneric #-}

module Types.User where


import Data.Text (Text)
import Data.Aeson
import GHC.Generics


-- * Profile

data Profile = Profile
    { avatar_hash           :: !Text
    , image_24              :: !Text
    , image_32              :: !Text
    , image_48              :: !Text
    , image_72              :: !Text
    , image_192             :: !Text
    , image_512             :: !Text
    , image_1024            :: !Text
    , image_original        :: !Text
    , real_name             :: !Text
    , real_name_normalized  :: !Text
    , email                 :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON Profile
instance FromJSON Profile


-- * User

data User = User
    { id                    :: !Text
    , name                  :: !Text
    , color                 :: !Text
    , deleted               :: !Bool
    , is_admin              :: !Bool
    , is_owner              :: !Bool
    , is_primary_owner      :: !Bool
    , is_restricted         :: !Bool
    , is_ultra_restricted   :: !Bool
    , is_bot                :: !Bool
    , profile               :: !Profile
    } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
