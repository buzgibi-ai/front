module TTHouse.Component.Copyright (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Component.Utils (initTranslation)
import TTHouse.Component.Subscription.Translation as Translation
import TTHouse.Api.Foreign.Scaffold as Scaffold

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Type.Proxy (Proxy (..))

proxy = Proxy :: _ "copyright"

loc = "TTHouse.Component.Copyright"

data Action = Initialize | LangChange String String

type State = { copyright :: String, hash :: String }

component =
  H.mkComponent
    { initialState: const { copyright: mempty, hash: mempty }
    , render: render
    , eval: H.mkEval H.defaultEval 
      { initialize = pure Initialize
      , handleAction = handleAction }
    }
  where
    handleAction Initialize = do
      void $ initTranslation loc \hash translation ->
        H.modify_ _ { copyright = Scaffold.getTranslationCopyright translation }
      Translation.load loc $ \hash translation -> 
        handleAction $ LangChange hash $ Scaffold.getTranslationCopyright translation  
    handleAction (LangChange _ new) = H.modify_ _ { copyright = new } 

render { copyright } = HH.div [css "copyright-plaque"] [HH.text copyright]