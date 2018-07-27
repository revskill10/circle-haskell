{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( app
    , User(..)
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Proxy
import qualified Data.Text              as T
import           Env
import           GHC.Generics           (Generic)
import qualified Lucid                  as L
import qualified Lucid.Base             as L
import           Miso
import           Network.Wai
import           Servant
import           Servant.API
import           Servant.Auth.Server


type HtmlRoutes = ToServerRoutes ViewRoutes HtmlPage Action
type ViewRoutes = Home
data Action = NoOp
      deriving (Show, Eq)
type Home = View Action

homeLink :: URI
homeLink =
    linkURI $
    safeLink (Proxy :: Proxy ViewRoutes) (Proxy :: Proxy Home)

type Title = T.Text
newtype Meta = MkMeta Title
  deriving (Show, Eq)

data HtmlPage a =
  MkHtmlPage Meta a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
  toHtmlRaw = L.toHtml
  toHtml (MkHtmlPage (MkMeta _title) x) =
    L.doctypehtml_ $ do
      L.head_ $ do
        L.title_  $ L.toHtml _title
        L.meta_ [L.charset_ "utf-8"]
        L.with
          (L.script_ mempty)
          [ L.makeAttribute "src" "/all.min.js"
          , L.makeAttribute "async" mempty
          , L.makeAttribute "defer" mempty
          ]
      L.body_ (L.toHtml x)



api Development = Proxy :: Proxy (API '[JWT, Cookie] 'Development)
api Test        = Proxy :: Proxy (API '[JWT, Cookie] 'Test)
api Production  = Proxy :: Proxy (API '[JWT, Cookie] 'Production)



type UnprotectedAPI =
  "login" :> LoginAPI :<|> Raw

unprotected :: CookieSettings -> JWTSettings -> Server UnprotectedAPI
unprotected cs jwts = checkCreds cs jwts :<|> serveDirectoryFileServer "client/static"


type LoginAPI = ReqBody '[JSON] Login
              :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                   , Header "Set-Cookie" SetCookie]
                          NoContent)
checkCreds :: CookieSettings
           -> JWTSettings
           -> Server LoginAPI
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
  deriving (Eq, Show, Generic)

data User = User { _name :: String, _email :: String }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

instance ToJSON Login
instance FromJSON Login

checkJWT :: MonadIO m => User -> m (Maybe Bool)
checkJWT (User "alice" "alice@gmail.com") = return $ Just True
checkJWT _                                = return Nothing


type ProtectedAPI = (ReaderAPI :<|> HtmlRoutes)
protectedServer :: String -> AuthResult User -> Server ProtectedAPI
protectedServer xs a =
  readerServer xs a :<|> htmlServer xs a

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

htmlServer xs user = homeHtmlServer
  where homeHtmlServer =
          pure $ MkHtmlPage (MkMeta "KMS") "Home"


type API auths (env :: Env)
  = Auth auths User :> ProtectedAPI :<|> UnprotectedAPI
server :: String -> CookieSettings -> JWTSettings -> Server (API auths env)
server dbConf cs jwts = protectedServer dbConf :<|> unprotected cs jwts
-- nt :: Config -> AppM a -> Handler a
--nt conf x = runReaderT x conf
app
  :: (HasContextEntry context CookieSettings,
      HasContextEntry context JWTSettings) =>
     Env -> String -> JWTSettings -> Context context -> Application
app env dbConf jwtCfg cfg = serveWithContext (api env) cfg (server dbConf defaultCookieSettings jwtCfg)--hoistServer api (nt conf) server

