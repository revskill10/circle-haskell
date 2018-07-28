{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Handlers.Reader
where

import           Control.Monad.Except    (throwError)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans     (lift)
import           Handlers.Reader.Helpers (checkJWT)
import           Handlers.Types
import           Servant.API
import           Servant.Auth.Server     (AuthResult (..), throwAll)
import           Servant.Server          (err401)
type ReaderAPI = "ep1" :> Get '[JSON] Int
            :<|> "ep2" :> Get '[JSON] String
readerServer :: AuthResult User -> Server ReaderAPI
readerServer (Authenticated user) =
  withLog user (return 1797) :<|> withLog user (return (_name user))
  where
    withLog user action = do
      usr <- liftIO $ checkJWT user
      liftIO $ print user
      case usr of
        Just True -> action
        _         -> lift $ throwError err401

readerServer _ = lift (throwError err401) :<|> lift (throwError err401)
