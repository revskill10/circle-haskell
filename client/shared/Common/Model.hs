{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Common.Model
where
import           Control.Lens
import           Data.Aeson
import           GHC.Generics
import           Miso
import           Miso.String

newtype Model = Model { _val :: Int }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

initialModel = Model { _val = 0 }

makeLenses ''Model
