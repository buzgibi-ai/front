module Buzgibi.Component.HTML.Header (html) where

import Prelude

import Buzgibi.Component.Menu.Hamburger as Hamburger
import Buzgibi.Component.Menu.Navbar as Navbar
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Lang as Lang
import Buzgibi.Component.Async as Async
import Buzgibi.Data.Route (Route (Home))
import Buzgibi.Component.Menu.Anchors as Anchors 

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store.Types (Platform(..))

html route pl w =
  HH.header_
  [
      HH.nav [css "navbar"]
      [
          HH.div [css "container"]
          [
              HH.div [css "logo"] [HH.img [HPExt.src "images/logo-gradient.png"]]
          ,   HH.div_ [ HH.slot_ Anchors.proxy 1 (Anchors.component (route == Home)) unit ]
          ,   showMenu route pl w
          -- , HH.div [ css "lang-container" ] [ HH.div_ [ HH.slot_ Lang.proxy unit Lang.component unit ] ]
          ,   HH.div_ [ HH.slot_ Async.proxy 3 Async.component unit ]
          ]
      ]
  ]

showMenu route Mobile _ = HH.slot_ Hamburger.proxy 2 Hamburger.component { route: route }
showMenu route _ w
  | w > 500 = HH.slot_ Navbar.proxy 2 Navbar.component { route: route }
  | otherwise = HH.slot_ Hamburger.proxy 2 Hamburger.component { route: route }