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


readerApi = Proxy :: Proxy ReaderAPI
readerServer = return 1797 :<|> ask
nt x = return (runReader x "hi")
mainServer = hoistServer readerApi nt readerServer :: Server ReaderAPI

app :: Application
app = serve readerApi mainServer

