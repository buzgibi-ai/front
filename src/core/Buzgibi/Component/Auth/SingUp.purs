module Buzgibi.Component.Auth.SignUp (component, proxy) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "auth_signUp"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render = HH.text "sign up"