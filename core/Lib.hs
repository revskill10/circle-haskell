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
import Control.Monad.IO.Class (liftIO)
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
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
   -- Usually you would ask a database for the user info. This is just a
   -- regular servant handler, so you can follow your normal database access
   -- patterns (including using 'enter').
   let usr = User "alice" "alice@gmail.com"
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError err401

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

checkJWT (User "alice" "alice@gmail.com") = True
checkJWT _ = False

-- readerServer :: AuthResult User -> Server ReaderAPI
readerServer xs (Authenticated user) = 
  if (checkJWT user) 
  then (return 1797) :<|> (return (name user <> xs)) 
  else throwAll err401
readerServer xs _ = throwAll err401          

-- server :: CookieSettings -> JWTSettings -> ReaderT String (Server (API auths))
server dbConf cs jwts = (readerServer dbConf) :<|> (unprotected cs jwts)

-- nt :: Config -> AppM a -> Handler a
--nt conf x = runReaderT x conf

-- app :: Config -> Application
app dbConf jwtCfg cfg = serveWithContext api cfg (server dbConf defaultCookieSettings jwtCfg)--hoistServer api (nt conf) server

