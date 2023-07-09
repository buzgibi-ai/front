module Buzgibi.Component.Menu.Hamburger ( component, proxy ) where

import Prelude

import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Data.Route (Route (..))
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Menu.Navbar ( mkItem )
import Buzgibi.Component.Subscription.Logout as Logout

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
import Halogen.Store.Monad (getStore)
import Data.Maybe (isJust)
import Data.Array (concatMap)

proxy = Proxy :: _ "hamburger"

loc = "Buzgibi.Component.HTML.Menu.Hamburger"

data Action = Initialize | LangChange String (Map.Map String String) | Finalize | ShowAuth

type State = 
     { route :: Route
     , menu :: Map.Map String String
     , hash :: String
     , isAuth :: Boolean
     }

component =
  H.mkComponent
    { initialState:
      \{ route } -> { route: route, menu: Map.empty, hash: mempty, isAuth: false }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
    }
    where 
      handleAction Initialize = do
        {user} <- getStore
        void $ initTranslation loc \hash translation -> 
          H.modify_ _ { 
              hash = hash
            , menu = BuzgibiBack.getTranslationMenu translation
            , isAuth = isJust user  }
        { menu, hash } <- H.get
        logDebug $ loc <> " ---> " <> show (Map.keys menu)
        logDebug $ loc <> " hash: ---> " <> hash
        Translation.subscribe loc $ \hash translation -> 
          handleAction $ LangChange hash $ BuzgibiBack.getTranslationMenu translation
        Logout.subscribe loc $ handleAction ShowAuth   
      handleAction (LangChange hash xs) = do 
        logDebug $ loc <> " ---> " <> show xs
        logDebug $ loc <> " hash: ---> " <> hash
        H.modify_ _ { hash = hash, menu = xs }
      handleAction Finalize = logDebug $ loc <> " ---> hamburger vanished"
      handleAction ShowAuth = H.modify_ _ { isAuth = false }

-- I piggyback on the following implementation https://codepen.io/alvarotrigo/pen/PoJGObg
render { route, menu, isAuth } = HH.div_ [HH.ul_ (concatMap (mkItem isAuth route menu addFontStyle) (fromEnum Home .. fromEnum SignIn) )]   
  
addFontStyle el = HH.div [] [el]