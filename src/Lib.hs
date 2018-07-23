{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings #-}
module Lib
    ( app
    , 
    ) where

import Servant
import Servant.API
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Data.Proxy
import Network.Wai

type ReaderAPI = "ep1" :> Get '[JSON] Int :<|> "ep2" :> Get '[JSON] String

type Config = String
type AppM = ReaderT Config Handler

readerApi = Proxy :: Proxy ReaderAPI

readerServer :: ServerT ReaderAPI AppM
readerServer = return 1797 :<|> ask

nt :: Config -> AppM a -> Handler a
nt conf x = runReaderT x conf

app :: Config -> Application
app conf = serve readerApi $ hoistServer readerApi (nt conf) readerServer

