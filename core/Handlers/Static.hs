{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Handlers.Static
where

import           Handlers.Static.Css        (CssAPI, cssServer)
import           Servant.API                ((:<|>) (..), (:>), Raw)
import           Servant.Server.StaticFiles (serveDirectoryFileServer)

type StaticAPI = "styles.css" :> CssAPI :<|> Raw
staticServer = cssServer :<|> serveDirectoryFileServer "client/static"
