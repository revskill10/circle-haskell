{-# LANGUAGE OverloadedStrings #-}
module Server.Helpers
where

import           App                (app, generateAppConfig)
import           Protolude
import           System.Environment (lookupEnv)

generateServerApp = do
  js_base <- fetchEnv "/static" "JS_BASE"
  (cfg, ctx) <- generateAppConfig js_base
  let serverApp = app cfg ctx
  return serverApp

fetchPort :: Int -> IO Int
fetchPort defaultPort = do
  port <- fetchEnv defaultPort "PORT"
  when (invalidPort port) exitFailure
  pure port

invalidPort port = port >= 49151 || port <= 1024

fetchEnv defaultVal envVar = fmap (fromMaybe defaultVal . (>>= readMaybe)) (lookupEnv envVar)
