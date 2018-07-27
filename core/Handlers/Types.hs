{-# LANGUAGE DeriveGeneric #-}

module Handlers.Types
where

import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Servant.Auth.Server (FromJWT, ToJWT)

data User = User { _name :: String, _email :: String }
    deriving (Eq, Show, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
