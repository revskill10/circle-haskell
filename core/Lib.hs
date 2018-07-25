{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings, DeriveGeneric, FlexibleContexts #-}
module Lib
    ( app
    , User(..)
    ) where

import Servant
import Servant.API
import Data.Proxy
import Network.Wai
import Servant.Auth.Server
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO, MonadIO)
import API(test)

type ReaderAPI = "ep1" :> Get '[JSON] Int 
            :<|> "ep2" :> Get '[JSON] String

type Unprotected =
    "login"
        :> ReqBody '[JSON] Login
        :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                           , Header "Set-Cookie" SetCookie]
                                  NoContent)
        :<|> Raw

type API auths = (Auth auths User :> ReaderAPI) :<|> Unprotected

api = Proxy :: Proxy (API '[JWT, Cookie])

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = checkCreds cs jwts :<|> serveDirectoryFileServer "example/static"

checkCreds :: CookieSettings
           -> JWTSettings
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                               NoContent)
checkCreds cookieSettings jwtSettings login = do
  user <- liftIO $ authenticate login
  case user of
    Nothing -> throwError err401
    Just usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

authenticate :: MonadIO m => Login -> m (Maybe User)
authenticate (Login "truong" "dung") = pure $ Just (User "alice" "alice@gmail.com")
authenticate _ = pure Nothing

data Login = Login { username :: String, password :: String }
  deriving (Eq, Show, Read, Generic)

data User = User { name :: String, email :: String }
  deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

instance ToJSON Login
instance FromJSON Login

checkJWT :: MonadIO m => User -> m (Maybe Bool)
checkJWT (User "alice" "alice@gmail.com") = return $ Just True
checkJWT _ = return Nothing

-- readerServer :: AuthResult User -> Server ReaderAPI
readerServer xs (Authenticated user) =
  withLog user (return 1797) :<|> withLog user (return (name user <> xs)) 
  where
    withLog user action = do
      usr <- liftIO $ checkJWT user
      -- liftIO $ print user
      case usr of
        Just True -> action
        _ -> throwError err401
    
readerServer xs a@(_) = throwAll err401          

-- server :: CookieSettings -> JWTSettings -> ReaderT String (Server (API auths))
server dbConf cs jwts = (readerServer dbConf) :<|> (unprotected cs jwts)

-- nt :: Config -> AppM a -> Handler a
--nt conf x = runReaderT x conf

-- app :: Config -> Application
app dbConf jwtCfg cfg = serveWithContext api cfg (server dbConf defaultCookieSettings jwtCfg)--hoistServer api (nt conf) server

