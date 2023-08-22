module Buzgibi.Component.Menu.Anchors (component, proxy, Anchor (..), mkPrintAnchor) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
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

proxy = Proxy :: _ "hamburger"

loc = "Buzgibi.Component.HTML.Menu.Hamburger"

type State = { isAuth :: Boolean }

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

data Action = Initialize

component isHome =
  H.mkComponent
    { initialState: const { isAuth: false }
    , render: render isHome
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
    where 
      handleAction Initialize = do
        { user } <- getStore
        H.modify_ _ { isAuth = isJust user }

render true { isAuth: false } = 
  HH.div [css "nav-links"] 
  [
      HH.ul_ $ (fromEnum Home .. fromEnum HowItWorks) <#> \idx -> 
        HH.li_ [HH.a [css "nav-link", HPExt.href ("#" <> mkPrintAnchor (mkAnchor idx))] [HH.text (print (mkAnchor idx))]]
  ]
render _ _ = HH.div_ []