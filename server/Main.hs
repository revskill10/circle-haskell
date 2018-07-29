module Main where

import           Handlers.Types           (generateAppConfig)
import           Lib                      (app)
import qualified Network.Wai.Handler.Warp as Wai

main :: IO ()
main = do
    (cfg, ctx) <- generateAppConfig
    Wai.run 3003 (Lib.app cfg ctx)
