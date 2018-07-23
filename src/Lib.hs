{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings #-}
module Lib
    ( app
    , 
    ) where

import Servant
import Servant.API
import Control.Monad.Reader
import Data.Proxy
import Network.Wai

type ReaderAPI = "ep1" :> Get '[JSON] Int :<|> "ep2" :> Get '[JSON] String

type Config = String

readerApi = Proxy :: Proxy ReaderAPI
readerServer = return 1797 :<|> ask
nt x = return (runReader x "hi")
mainServer x = hoistServer readerApi (nt x) readerServer :: Server ReaderAPI

app :: Config -> Application
app conf = serve readerApi (mainServer conf)

