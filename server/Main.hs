module Main where

import           App                      (app, generateAppConfig)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)

main :: IO ()
main = do
    (cfg, ctx) <- generateAppConfig
    let mainApp = app cfg ctx
    withStdoutLogger $ \aplogger -> do
      let settings = setPort 3003 $ setLogger aplogger defaultSettings
      runSettings settings mainApp
