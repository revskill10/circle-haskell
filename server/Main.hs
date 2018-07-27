module Main where

import           Env                      (getEnv)
import           Lib                      (app)
import qualified Network.Wai.Handler.Warp as Wai
import           Servant.Auth.Server
import           Servant.Server

main :: IO ()
main = do
    myKey <- generateKey
    let dbConf = "hi"
        jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings
        cfg = cookieCfg :. jwtCfg :. EmptyContext
        devEnv = getEnv "DEVELOPMENT"
    Wai.run 3003 (Lib.app devEnv dbConf cookieCfg jwtCfg cfg)
