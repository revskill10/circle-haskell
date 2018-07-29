{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.Helpers
where

import           App                (app, generateAppConfig)
import           Data.Text
import           Protolude
import           System.Environment (lookupEnv)

generateServerApp = do
  js_base <- fetchJsBase "/static"
  print $ "JS_BASE is " <> js_base
  (cfg, ctx) <- generateAppConfig js_base
  let serverApp = app cfg ctx
  return serverApp

fetchJsBase :: Text -> IO Text
fetchJsBase defaultJsBase =
  maybe defaultJsBase pack <$> lookupEnv "JS_BASE"

fetchPort :: Int -> IO Int
fetchPort defaultPort = do
  port <- fetchEnv defaultPort "PORT"
  when (invalidPort port) exitFailure
  pure port

invalidPort port = port >= 49151 || port <= 1024

fetchEnv defaultVal envVar = fmap (fromMaybe defaultVal . (>>= readMaybe)) (lookupEnv envVar)
