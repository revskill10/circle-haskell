{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Handlers.Login
where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.Trans    (lift)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Handlers.Types
import           Servant.API            ((:>), Header, Headers, JSON,
                                         NoContent (..), PostNoContent, ReqBody)
import           Servant.Auth.Server    (CookieSettings, JWTSettings, SetCookie,
                                         acceptLogin)
import           Servant.Server         hiding (Server)

data Login          = Login { username :: Text, password :: Text }
                        deriving (Eq, Show, Generic, FromJSON, ToJSON)
type CookieHeader   = Header "Set-Cookie" SetCookie
type LoginResponse  = Headers '[CookieHeader, CookieHeader] NoContent
type LoginAPI       = ReqBody '[JSON] Login :> PostNoContent '[JSON] LoginResponse

loginServer :: Server LoginAPI
loginServer login = do
  cookieSettings <- asks _cookieCfg
  jwtSettings <- asks _jwtCfg
  user <- liftIO $ authenticate login

  case user of
    Nothing -> lift $ throwError err401
    Just usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing           -> lift $ throwError err401
        Just applyCookies -> return $ applyCookies NoContent

authenticate :: MonadIO m => Login -> m (Maybe User)
authenticate (Login "truong" "dung") = pure $ Just (User "alice" "alice@gmail.com")
authenticate _ = pure Nothing
