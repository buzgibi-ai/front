module TTHouse.Component.AppInitFailure (component) where

import Prelude

import TTHouse.Component.HTML.Utils (css)
import TTHouse.Component.Utils (initTranslation)
import TTHouse.Component.Subscription.Translation as Translation
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Component.HTML.Error50x (html) 

import Halogen as H
import Halogen.HTML as HH
import Undefined
import Effect.Exception (Error, message)
import Halogen.HTML.Properties.Extended as HPExt

type State = { error ::  Error }

component = H.mkComponent { initialState: identity , render: html <<< message <<< _.error, eval: H.mkEval H.defaultEval } 