module Buzgibi.Page.Home.Anchors (component, proxy, Anchor(..), mkPrintAnchor) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events (onClick)
import Halogen.Store.Monad (getStore)
import Data.Maybe (isJust)
import Data.Map as Map
import Data.Array ((..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Enum
import Data.Maybe
import Data.Bounded
import Data.Enum.Generic (genericFromEnum, genericToEnum, genericSucc, genericPred, genericCardinality)
import Undefined (undefined)
import Type.Proxy (Proxy(..))
import Web.Element (scrollTo)

proxy = Proxy :: _ "home_anchors"

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

mkPrintAnchor Home = "home"
mkPrintAnchor Advanteges = "advanteges"
mkPrintAnchor HowItWorks = "how"

data Action = Scroll String

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  handleAction (Scroll el) = H.liftEffect $ scrollTo el

render =
  HH.div [ css "nav-links" ]
    [ HH.ul_ $ (fromEnum Home .. fromEnum HowItWorks) <#> \idx ->
        HH.li_ [ HH.div [ css "nav-link", HPExt.style "cursor:pointer", onClick $ const $ Scroll (mkPrintAnchor (mkAnchor idx)) ] [ HH.text (print (mkAnchor idx)) ] ]
    ]