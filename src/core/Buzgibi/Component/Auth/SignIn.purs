module Buzgibi.Component.Auth.SignIn
  ( component
  , proxy
  , slot
  )
  where

import Prelude

import Buzgibi.Data.Route as Route
import Buzgibi.Capability.Navigate (navigate)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.Store.Monad (getStore)
import Data.Maybe (isJust)

proxy = Proxy :: _ "auth_signIn"

loc = "Buzgibi.Component.Auth.SignIn"

data Action = MakeRequest Event | Initialize

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
    where 
      handleAction Initialize = do
        {jwtUser} <- getStore
        when (isJust jwtUser) $ navigate Route.Home
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev

render = HH.text "sign in"

slot = HH.slot_ proxy unit component unit