module Buzgibi.Component.Auth.User
  ( component
  , proxy
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe (..))
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)

import Undefined

loc = "Buzgibi.Component.Auth.User"

proxy = Proxy :: _ "auth_user"

type State = { email :: Maybe String }

data Action = Initialize | MakeRequest Event

component =
  H.mkComponent
    { initialState: const {email: Nothing}
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
    where 
      handleAction Initialize = do
        {jwtUser} <- getStore 
        for_ jwtUser $ const $ H.modify_ _ { email = Just "test" }
      handleAction (MakeRequest ev) = H.liftEffect $ preventDefault ev

render { email: Just email } = 
  HH.div_ 
  [
      HH.text email
  ,   HH.form 
      [ HE.onSubmit MakeRequest] [HH.input 
      [ HPExt.type_ HPExt.InputSubmit, HPExt.value "logout"]] 
  ]
render { email: Nothing } = HH.div_ []