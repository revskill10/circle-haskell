{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Protolude
import           Server.Helpers

main :: IO ()
main = do
    mainApp <- generateServerApp
    port <- fetchPort 3003
    print $ "PORT is " <> show port
    withStdoutLogger $ \aplogger -> do
      let settings = setPort port $ setLogger aplogger defaultSettings
      runSettings settings mainApp
