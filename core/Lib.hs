{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( app
    , User(..)
    ) where

import           Data.Proxy
import           Env
import           Handlers.Html
import           Handlers.Login
import           Handlers.Reader
import           Handlers.Types
import           Network.Wai
import           Servant.API
import           Servant.Auth.Server
import           Servant.Server             hiding (Server)
import           Servant.Server.StaticFiles (serveDirectoryFileServer)


type UnprotectedAPI = "login" :> LoginAPI :<|> Raw
unprotectedServer :: Server UnprotectedAPI
unprotectedServer = loginServer :<|> serveDirectoryFileServer "client/static"

type ProtectedAPI = "api" :> ReaderAPI :<|> HtmlAPI
protectedServer :: AuthResult User -> Server ProtectedAPI
protectedServer a = readerServer a :<|> htmlServer a

type API auths = Auth auths User :> ProtectedAPI :<|> UnprotectedAPI
apiServer :: Server (API auths)
apiServer = protectedServer  :<|> unprotectedServer

apiProxy = Proxy :: Proxy (API '[Cookie, JWT])

app cookieCfg jwtCfg cfg = serveWithContext apiProxy cfg (nt (AppConfig cookieCfg jwtCfg) apiProxy apiServer)
