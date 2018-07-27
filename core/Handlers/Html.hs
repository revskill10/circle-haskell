{-# LANGUAGE OverloadedStrings #-}

module Handlers.Html
where

import           Handlers.Html.Types (Action, HtmlPage (..), Meta (..),
                                      ViewRoutes)
import           Handlers.Types      (User)
import           Miso                (ToServerRoutes)
import           Servant.Auth.Server (AuthResult)
import           Servant.Server      (Server)

type HtmlAPI = ToServerRoutes ViewRoutes HtmlPage Action

htmlServer :: String -> AuthResult User -> Server HtmlAPI
htmlServer xs user = homeHtmlServer
    where homeHtmlServer =
            pure $ MkHtmlPage (MkMeta "KMS") "Home"
