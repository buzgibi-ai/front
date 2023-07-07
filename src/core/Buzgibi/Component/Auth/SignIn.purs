module Buzgibi.Component.Auth.SignIn
  ( component
  , proxy
  , slot
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

proxy = Proxy :: _ "auth_signIn"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render = HH.text "sign in"

slot = HH.slot_ proxy unit component unit