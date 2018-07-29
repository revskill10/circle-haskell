{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Helpers
where

import           App                   (User (..), app, generateAppConfig,
                                        _jwtCfg)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Network.HTTP.Types
import           Servant.Auth.Server   (makeJWT)


bearerType jwt = BC.pack "Bearer " <> BL.toStrict jwt

headers :: BL.ByteString -> [Header]
headers jwt = [
    (hAuthorization,  bearerType jwt)
    ]

jsonHeader :: [Header]
jsonHeader = [
    (hContentType, "application/json")
    ]

validUser = User "alice" "alice@gmail.com"
invalidUser = User "test" "fdsfd@fdsfd.com"

mkHeaders user jwtCfg = do
    jwtRes <-  makeJWT user jwtCfg Nothing
    case jwtRes of
      Left _    -> return $ headers ""
      Right jwt -> return $ headers jwt

generateMockApp = do
  (cfg, ctx) <- generateAppConfig "/static"
  let testApp = app cfg ctx
      jwtCfg = _jwtCfg cfg
  return (testApp, jwtCfg)
