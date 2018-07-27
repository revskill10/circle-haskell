{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Handlers.Html.Types
where
import           Data.Proxy

import           Data.Text
import           Lucid
import           Lucid.Base    (makeAttribute, toHtml)
import           Miso          (View)
import           Servant.API   (URI, safeLink)
import           Servant.Links (linkURI)


type ViewRoutes = Home
data Action = NoOp
      deriving (Show, Eq)
type Home = View Action

homeLink :: URI
homeLink = linkURI $ safeLink (Proxy @ViewRoutes) (Proxy @Home)

type Title = Text
newtype Meta = MkMeta Title
  deriving (Show, Eq)

data HtmlPage a =
  MkHtmlPage Meta a
  deriving (Show, Eq)

instance ToHtml a => ToHtml (HtmlPage a) where
  toHtmlRaw = toHtml
  toHtml (MkHtmlPage (MkMeta _title) x) =
    doctypehtml_ $
      head_ $ do
        title_  $ toHtml _title
        meta_ [charset_ "utf-8"]
        with
          (script_ mempty)
          [ makeAttribute "src" "/all.min.js"
          , makeAttribute "async" mempty
          , makeAttribute "defer" mempty
          ]
        body_ (toHtml x)
