{-# LANGUAGE OverloadedStrings #-}

module Handlers.Html
where

import           Common.Action
import           Common.Model
import           Common.View
import           Control.Monad.Reader (asks)
import           Data.Semigroup       ((<>))
import           Handlers.Html.Types  (HtmlPage (..), JS (..), Meta (..),
                                       ViewRoutes)
import           Handlers.Types
import           Miso
import           Miso.String
import           Servant.Auth.Server  (AuthResult)

type HtmlAPI = ToServerRoutes ViewRoutes HtmlPage Action

htmlServer :: AuthResult User -> Server HtmlAPI
htmlServer user = homeHtmlServer

homeHtmlServer = do
  jsBase <- asks _jsBase
  let jsUrl = jsBase <> "/home.min.js"
      pageTitle = "KMS"
  pure $ MkHtmlPage (MkMeta "KMS") (MkJS jsUrl) (viewModel initialModel)
