{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Handlers.Json
where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans    (lift)
import           Handlers.Json.GraphQL
import           Handlers.Json.Helpers  (checkJWT)
import           Handlers.Types
import           Servant.API
import           Servant.Auth.Server    (AuthResult (..), throwAll)
import           Servant.Server         (err401)
type JsonAPI = "ep1" :> Get '[JSON] Int
            :<|> "ep2" :> Get '[JSON] String
            :<|> "graphql" :> GraphQLAPI
jsonServer :: AuthResult User -> Server JsonAPI
jsonServer (Authenticated user) =
  withLog user (return 1797) :<|> withLog user (return (_name user)) :<|> graphQLServer user
  where
    withLog user action = do
      usr <- liftIO $ checkJWT user
      liftIO $ print user
      case usr of
        Just True -> action
        _         -> lift $ throwError err401

jsonServer _ = throwAll err401 --undefined --lift (throwError err401) :<|> lift (throwError err401) :<|> graphQLServerError
