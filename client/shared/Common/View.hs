{-# LANGUAGE OverloadedStrings #-}
module Common.View
where

import           Common.Action
import           Common.Model
import           Miso
import           Miso.String
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text $ ms (show (_val x))
 , button_ [ onClick SubtractOne ] [ text "+fdsfsdf+Dung" ]
 ]
