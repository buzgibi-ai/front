module Buzgibi.Component.Menu.Navbar ( component, proxy, mkItem ) where

import Prelude

import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Data.Route (Route (..))
import Buzgibi.Component.Lang.Data (Lang (..))
import Buzgibi.Capability.LogMessages (logDebug, logError)
import Buzgibi.Component.Subscription.Translation as Translation 
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Utils (initTranslation)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import Type.Proxy (Proxy(..))
import Effect.Aff as Aff
import Data.Map as Map
import Data.Maybe (Maybe (..), fromMaybe)

import Undefined


proxy = Proxy :: _ "navbar"

loc = "Buzgibi.Component.HTML.Menu.Navbar"

data Action = Initialize | LangChange String (Map.Map String String)

type State = 
     { route :: Route
     , menu :: Map.Map String String
     , hash :: String
     }

component =
  H.mkComponent
    { initialState: 
      \{ route } -> { route: route, menu: Map.empty, hash: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      }
    }
    where 
      handleAction Initialize = do
        void $ initTranslation loc \hash translation -> 
          H.modify_ _ {
              menu = BuzgibiBack.getTranslationMenu translation
            , hash = hash }
        { menu, hash } <- H.get
        logDebug $ loc <> " menu: ---> " <> show (Map.keys menu)
        logDebug $ loc <> " hash: ---> " <> hash
        Translation.load loc $ \hash translation -> 
          handleAction $ LangChange hash $ BuzgibiBack.getTranslationMenu translation
      handleAction (LangChange hash xs) = do 
        logDebug $ loc <> " ---> " <> show xs
        logDebug $ loc <> " hash: ---> " <> hash
        H.modify_ _ { hash = hash, menu = xs }

-- taken from: https://codepen.io/albizan/pen/mMWdWZ
render { route, menu } = HH.div_ [HH.ul_ (map (mkItem route menu addFontStyle) (fromEnum SignUp .. fromEnum SignIn) )]

mkItem _ xs  _ _ | Map.isEmpty xs = HH.li_ [HH.text "loading.."]
mkItem route xs applyStyle idx =
  HH.li_ 
  [
      HH.a 
      [ safeHref (mkRoute idx)
      , isDisabled (mkRoute idx == route)
      ] 
      [el]
  ] 
  where 
    mkRoute = fromMaybe undefined <<< (toEnum :: Int -> Maybe Route)
    isDisabled true = HPExt.style "cursor: not-allowed;"
    isDisabled false = HPExt.style mempty
    title = fromMaybe (show (mkRoute idx)) $ Map.lookup (show (mkRoute idx)) xs
    el = applyStyle $ HH.text title

addFontStyle el = HH.div_ [el]
