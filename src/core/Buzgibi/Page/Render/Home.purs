module Buzgibi.Page.Render.Home (html) where

import Prelude

import Buzgibi.Data.Route as Route
import Buzgibi.Component.HTML.Utils (css, safeHref, whenElem)

import Halogen.HTML.Properties.Extended as HPExt
import Halogen.Html.Raw.Render as H
import Data.Maybe
import Halogen as H
import Halogen.HTML as HH
import Data.Map as Map
import Undefined

html constants isAuth =
  HH.div_
    [ H.render_ $ fromMaybe undefined $ Map.lookup "headline" constants
    , whenElem isAuth $ HH.div_
        [ HH.a
            [ css "nav-link"
            , HPExt.style "font-size: 30px"
            , safeHref Route.UserSurvey
            ]
            [ HH.text $ fromMaybe undefined $ Map.lookup "makeSurvey" constants ]
        ]
    ]