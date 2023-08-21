module Buzgibi.Component.HTML.Header (html) where

import Prelude

import Buzgibi.Component.Menu.Hamburger as Hamburger
import Buzgibi.Component.Menu.Navbar as Navbar
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Lang as Lang
import Buzgibi.Component.Async as Async
import Buzgibi.Data.Route (Route)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.Extended as HPExt
import Store.Types (Platform(..))
import Data.Map as Map
import Data.Array ((..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Enum
import Data.Maybe
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)
import Undefined (undefined)

data Anchor = Home | Advanteges | HowItWorks

derive instance Generic Anchor _ 

derive instance Eq Anchor
derive instance Ord Anchor

instance Enum Anchor where
  succ = genericSucc
  pred = genericPred

instance Bounded Anchor where
  top = HowItWorks
  bottom = Home

instance BoundedEnum Anchor where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show Anchor where
  show = genericShow

mkAnchor = fromMaybe undefined <<< (toEnum :: Int -> Maybe Anchor)

print Home = "home"
print Advanteges = "advantages"
print HowItWorks = "how it works"

html route pl w =
  HH.header_
  [
      HH.nav [css "navbar"]
      [
          HH.div [css "container"]
          [
              HH.div [css "logo"] [HH.img [HPExt.src "images/logo-gradient.png"]]
          ,   HH.div [css "nav-links"] 
              [
                  HH.ul_ ((fromEnum Home .. fromEnum HowItWorks) <#> \idx -> HH.li_ [HH.a [css "nav-link", HPExt.href ("#" <> show (mkAnchor idx))] [HH.text (print (mkAnchor idx))]])
              ]
          , showMenu route pl w
          -- , HH.div [ css "lang-container" ] [ HH.div_ [ HH.slot_ Lang.proxy unit Lang.component unit ] ]
          , HH.div_ [ HH.slot_ Async.proxy unit Async.component unit ]
          ]
      ]
  ]

showMenu route Mobile _ = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route }
showMenu route _ w
  | w > 500 = HH.slot_ Navbar.proxy unit Navbar.component { route: route }
  | otherwise = HH.slot_ Hamburger.proxy unit Hamburger.component { route: route }