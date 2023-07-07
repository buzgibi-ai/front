module Buzgibi.Component.Menu.Hamburger ( component, proxy ) where

import Prelude

import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Data.Route (Route (..))
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Menu.Navbar ( mkItem )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array ((..))
import Data.Enum (fromEnum, toEnum)
import DOM.HTML.Indexed.InputType
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Effect.Aff as Aff
import Data.Map as Map


proxy = Proxy :: _ "hamburger"

loc = "Buzgibi.Component.HTML.Menu.Hamburger"

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
              hash = hash
            , menu = BuzgibiBack.getTranslationMenu translation }
        { menu, hash } <- H.get
        logDebug $ loc <> " ---> " <> show (Map.keys menu)
        logDebug $ loc <> " hash: ---> " <> hash
        Translation.load loc $ \hash translation -> 
          handleAction $ LangChange hash $ BuzgibiBack.getTranslationMenu translation
      handleAction (LangChange hash xs) = do 
        logDebug $ loc <> " ---> " <> show xs
        logDebug $ loc <> " hash: ---> " <> hash
        H.modify_ _ { hash = hash, menu = xs }
     

-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render { route, menu } =
    HH.div_
    [
        HH.input [HPExt.type_ InputCheckbox, css "toggler"]
    ,   HH.div [css "hamburger"] [HH.div_ []]
    ,   HH.div [css "menu"]
        [
            HH.div [HPExt.style "#position: relative; #top: -50%;margin:0 auto;width:200px"] 
            [HH.ul_ (map (mkItem route menu addFontStyle) (fromEnum SignUp .. fromEnum SignIn) )]
        ]     
    ]

addFontStyle el = HH.div [HPExt.style "text-transform:uppercase;"] [el]