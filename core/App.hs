{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module App
(
  module Handlers.Types
, app
, generateAppConfig
)
where

import           Data.Proxy          (Proxy (..))
import           Data.Text
import           Handlers            (API, apiServer)
import           Handlers.Types
import           Servant.Auth.Server
import           Servant.Server      (Context (..), serveWithContext)

apiProxy = Proxy :: Proxy (API '[Cookie, JWT])

app cfg ctx = serveWithContext apiProxy ctx (nt cfg apiProxy apiServer)

-- generateAppConfig :: IO (AppConfig, Context ctx)
generateAppConfig jsBase = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = AppConfig { _jwtCfg = jwtCfg, _cookieCfg = cookieCfg, _jsBase = jsBase }
      ctx = cookieCfg :. jwtCfg :. EmptyContext
  return (cfg, ctx)

