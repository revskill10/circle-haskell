{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( app
    , User(..)
    ) where

import           Data.Proxy
import           Handlers.Html
import           Handlers.Login
import           Handlers.Reader
import           Handlers.Static
import           Handlers.Types
import           Servant.API
import           Servant.Auth.Server
import           Servant.Server      hiding (Server)


type UnprotectedAPI = "login" :> LoginAPI :<|> "static" :> StaticAPI
unprotectedServer :: Server UnprotectedAPI
unprotectedServer = loginServer :<|> staticServer

type ProtectedAPI = "api" :> ReaderAPI :<|> HtmlAPI
protectedServer :: AuthResult User -> Server ProtectedAPI
protectedServer a = readerServer a :<|> htmlServer a

type API auths = Auth auths User :> ProtectedAPI :<|> UnprotectedAPI
apiServer :: Server (API auths)
apiServer = protectedServer  :<|> unprotectedServer

apiProxy = Proxy :: Proxy (API '[Cookie, JWT])

app cookieCfg jwtCfg cfg = serveWithContext apiProxy cfg (nt (AppConfig cookieCfg jwtCfg) apiProxy apiServer)
