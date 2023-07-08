module Buzgibi.Component.HTML.Header ( html ) where

import Prelude

import Buzgibi.Component.Menu.Hamburger as Hamburger 
import Buzgibi.Component.Menu.Navbar as Navbar
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Lang as Lang
import Buzgibi.Component.Async as Async
import Buzgibi.Data.Route (Route)
import Buzgibi.Component.Auth.User as Auth.User

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store.Types (Platform (..))
import Data.Map as Map

html route pl w = 
  HH.div 
  [css "page-header"] 
  [
      HH.div [css "header-wrapper"] 
      [ 
          HH.div [css "header-logo-wrapper"]
          [HH.div_ [HH.slot_ Lang.proxy unit Lang.component unit]]
      ,   showMenu route pl w
      ,   HH.div_ [HH.slot_ Auth.User.proxy unit Auth.User.component unit]
      ,   HH.div_ [HH.slot_ Async.proxy unit Async.component unit]
      ]
  ]

showMenu route Mobile _ = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route }
showMenu route _ w 
  | w > 500 = HH.slot_ Navbar.proxy unit Navbar.component { route: route }
  | otherwise  = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route }