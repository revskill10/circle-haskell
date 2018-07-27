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
import           Servant
import           Servant.API
import           Servant.Auth.Server


api Development = Proxy @(API '[JWT, Cookie] 'Development)
api Test        = Proxy @(API '[JWT, Cookie] 'Test)
api Production  = Proxy @(API '[JWT, Cookie] 'Production)

type UnprotectedAPI = "login" :> LoginAPI :<|> Raw
unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedAPI
unprotectedServer cs jwts = loginServer cs jwts :<|> serveDirectoryFileServer "client/static"

type ProtectedAPI = ReaderAPI :<|> HtmlAPI
protectedServer :: String -> AuthResult User -> Server ProtectedAPI
protectedServer xs a = readerServer xs a :<|> htmlServer xs a

type API auths (env :: Env) = Auth auths User :> ProtectedAPI :<|> UnprotectedAPI
apiServer :: String -> CookieSettings -> JWTSettings -> Server (API auths env)
apiServer dbConf cs jwts = protectedServer dbConf :<|> unprotectedServer cs jwts

app :: (HasContextEntry context CookieSettings, HasContextEntry context JWTSettings)
    => Env -> String -> CookieSettings -> JWTSettings -> Context context -> Application
app env dbConf cookieCfg jwtCfg cfg = serveWithContext (api env) cfg (apiServer dbConf cookieCfg jwtCfg)
