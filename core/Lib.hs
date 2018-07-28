{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Lib
    ( app
    , User(..)
    ) where

import           Data.Proxy
import           Handlers.Html
import           Handlers.Json
import           Handlers.Login
import           Handlers.Static
import           Handlers.Types
import           Servant.API
import           Servant.Auth.Server
import           Servant.Server      hiding (Server)


type UnprotectedAPI = "login" :> LoginAPI :<|> "static" :> StaticAPI
unprotectedServer :: Server UnprotectedAPI
unprotectedServer = loginServer :<|> staticServer

type ProtectedAPI = "api" :> JsonAPI :<|> HtmlAPI
protectedServer :: AuthResult User -> Server ProtectedAPI
protectedServer a = jsonServer a :<|> htmlServer a

type API auths = Auth auths User :> ProtectedAPI :<|> UnprotectedAPI
apiServer :: Server (API auths)
apiServer = protectedServer  :<|> unprotectedServer

apiProxy = Proxy :: Proxy (API '[Cookie, JWT])

app cookieCfg jwtCfg cfg = serveWithContext apiProxy cfg (nt (AppConfig cookieCfg jwtCfg) apiProxy apiServer)
