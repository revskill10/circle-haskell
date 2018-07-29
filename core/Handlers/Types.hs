{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Handlers.Types
where
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Servant.Auth.Server
import           Servant.Server       (Context (..), Handler, ServerT,
                                       hoistServerWithContext)

data User = User { _name :: String, _email :: String }
    deriving (Eq, Show, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data AppConfig = AppConfig {
    _cookieCfg :: CookieSettings
  , _jwtCfg    :: JWTSettings
  , _jsBase    :: Text
  }

type AppM = ReaderT AppConfig Handler
type Server api = ServerT api AppM

transform :: AppConfig -> AppM a -> Handler a
transform s x = runReaderT x s

contextProxy = Proxy :: Proxy '[CookieSettings, JWTSettings]

nt cfg api = hoistServerWithContext api contextProxy (transform cfg)




