{-# LANGUAGE OverloadedStrings #-}
module Templates.LoginView
where

import           Miso
import           Miso.String

loginView = div_
  [ class_ "container" ]
  [ div_
    [ class_ "row" ]
    [ div_
      [ class_ "col-sm-4" ]
      [ label_
        []
        [ text "Current Password" ]
      , div_
        [ class_ "form-group pass_show" ]
        [ input_
          [ type_ "password"
          , value_ "faisalkhan@123"
          , class_ "form-control"
          , placeholder_ "Current Password" ]
        ]
      , label_
        []
        [ text "New Password" ]
      , div_
        [ class_ "form-group pass_show" ]
        [ input_
          [ type_ "password"
          , value_ "faisal.khan@123"
          , class_ "form-control"
          , placeholder_ "New Password" ]
        ]
      , label_
        []
        [ text "Confirm Password" ]
      , div_
        [ class_ "form-group pass_show" ]
        [ input_
          [ type_ "password"
          , value_ "faisal.khan@123"
          , class_ "form-control"
          , placeholder_ "Confirm Password" ]
        ]
      ]
    ]
  ]

