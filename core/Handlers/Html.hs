{-# LANGUAGE OverloadedStrings #-}

module Handlers.Html
where

import           Handlers.Html.Types (Action, HtmlPage (..), Meta (..),
                                      ViewRoutes)
import           Handlers.Types
import           Miso                (ToServerRoutes)
import           Servant.Auth.Server (AuthResult)

type HtmlAPI = ToServerRoutes ViewRoutes HtmlPage Action

htmlServer :: AuthResult User -> Server HtmlAPI
htmlServer user = homeHtmlServer
    where homeHtmlServer =
            pure $ MkHtmlPage (MkMeta "KMS") "Home"
