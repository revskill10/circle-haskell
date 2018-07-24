module Main where

import Lib (app)
import Servant.Auth.Server
import Servant.Server
import qualified Network.Wai.Handler.Warp as Wai

main :: IO ()
main = do
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    Wai.run 3003 (Lib.app jwtCfg cfg)
