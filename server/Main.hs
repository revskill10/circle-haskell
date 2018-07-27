module Main where

import           Lib                      (app)
import qualified Network.Wai.Handler.Warp as Wai
import           Servant.Auth.Server
import           Servant.Server           (Context (..))

main :: IO ()
main = do
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings
        cfg = cookieCfg :. jwtCfg :. EmptyContext
    Wai.run 3003 (Lib.app cookieCfg jwtCfg cfg)
