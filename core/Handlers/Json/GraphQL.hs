{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Handlers.Json.GraphQL
where

import           Control.Monad.Except (throwError)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Handlers.Types
import           Protolude
import           Servant.API
import           Servant.Server       (err401)

newtype IntData = IntData { intVal :: Int }
  deriving Generic

instance ToJSON IntData
instance FromJSON IntData

newtype TextData = TextData { textVal :: Text }
  deriving Generic

instance ToJSON TextData
instance FromJSON TextData

type GraphQLAPI1 = ReqBody '[JSON] IntData :> Post '[JSON] IntData
type GraphQLAPI2 = ReqBody '[JSON] TextData :> Post '[JSON] TextData
type GraphQLAPI = GraphQLAPI1 :<|> GraphQLAPI2

graphQLServer :: User -> Server GraphQLAPI
graphQLServer user = graphQLServer1 user :<|> graphQLServer2 user

graphQLServer1 :: User -> Server GraphQLAPI1
graphQLServer1 user inp = return (inp {intVal = intVal inp + 1 })

graphQLServer2 :: User -> Server GraphQLAPI2
graphQLServer2 user str = return (str { textVal = textVal str <> "2" })
