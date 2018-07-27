{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Handlers.Reader
where

import           Control.Monad.Except    (throwError)
import           Control.Monad.IO.Class  (liftIO)
import           Handlers.Types          (User (..))
import           Servant.API
import           Servant.Auth.Server     (AuthResult (..), throwAll)
import           Servant.Server          (Server, err401)

import           Handlers.Reader.Helpers (checkJWT)

type ReaderAPI = "ep1" :> Get '[JSON] Int
            :<|> "ep2" :> Get '[JSON] String
readerServer :: String -> AuthResult User -> Server ReaderAPI
readerServer xs (Authenticated user) =
  withLog user (return 1797) :<|> withLog user (return (_name user <> xs))
  where
    withLog user action = do
      usr <- liftIO $ checkJWT user
      liftIO $ print user
      case usr of
        Just True -> action
        _         -> throwError err401

readerServer xs _ = throwAll err401
