{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handlers.Static.Css
where

import           Clay
import           Handlers.Static.Css.Helper
import           Handlers.Types
import           Protolude
import           Servant.API                (Get)

headerStyle :: Css
headerStyle = header ?
  do background lightgrey
     paddingTop (em 1.5)

navStyle :: Css
navStyle = nav ?
  do  background (Other "#1abc9c")
      ul ? marginTop (px 0) <> paddingTop (px 0) <> listStyleType none
      li ?
        do display inlineBlock
           a ? display inlineBlock <> textDecoration none <> color white <> padding (px 14) (px 16) (px 14) (px 16)

type CssAPI = Get '[CSS] Css
cssServer :: Server CssAPI
cssServer = pure (headerStyle <> navStyle)
