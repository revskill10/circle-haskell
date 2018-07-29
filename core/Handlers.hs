{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Handlers
    ( API
    , apiServer
    ) where

import           Data.Proxy
import           Handlers.Html
import           Handlers.Json
import           Handlers.Login
import           Handlers.Static
import           Handlers.Types
import           Servant.API
import           Servant.Auth.Server

type UnprotectedAPI = "login" :> LoginAPI :<|> "static" :> StaticAPI
unprotectedServer :: Server UnprotectedAPI
unprotectedServer = loginServer :<|> staticServer

type ProtectedAPI = "api" :> JsonAPI :<|> HtmlAPI
protectedServer :: AuthResult User -> Server ProtectedAPI
protectedServer a = jsonServer a :<|> htmlServer a

type API auths = Auth auths User :> ProtectedAPI :<|> UnprotectedAPI
apiServer :: Server (API auths)
apiServer = protectedServer  :<|> unprotectedServer
