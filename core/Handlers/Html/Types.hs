{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Handlers.Html.Types
where
import           Data.Proxy

import           Common.Action
import           Data.Text
import           Lucid
import           Lucid.Base    (makeAttribute, toHtml)
import           Miso          (View)
import           Servant.API   (URI, safeLink)
import           Servant.Links (linkURI)

type ViewRoutes = Home
type Home = View Action

homeLink :: URI
homeLink = linkURI $ safeLink (Proxy @ViewRoutes) (Proxy @Home)

type Title = Text
newtype Meta = MkMeta Title
  deriving (Show, Eq)

type Path = Text
newtype JS = MkJS Path
  deriving (Show, Eq)

data HtmlPage a =
  MkHtmlPage Meta JS a
  deriving (Show, Eq)

bootstraCdnPath = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"

instance ToHtml a => ToHtml (HtmlPage a) where
  toHtmlRaw = toHtml
  toHtml (MkHtmlPage (MkMeta _title) (MkJS _path) x) =
    doctypehtml_ $
      head_ $ do
        title_  $ toHtml _title
        meta_ [charset_ "utf-8"]
        link_ [rel_ "shortcut icon", href_ ""]
        link_ [rel_ "stylesheet", type_ "text/css", href_ bootstraCdnPath]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css"]
        with
          (script_ mempty)
          [ makeAttribute "src" _path
          , makeAttribute "async" mempty
          , makeAttribute "defer" mempty
          ]
        body_ (toHtml x)
