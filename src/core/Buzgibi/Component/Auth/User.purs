module Buzgibi.Component.Auth.User
  ( component
  , proxy
  )
  where

import Prelude

import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack 
import Buzgibi.Data.Config (Config (..))
import Buzgibi.Api.Foreign.Request.Handler (withError) 

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe (..))
import Halogen.Store.Monad (getStore, updateStore)
import Data.Foldable (for_)
import Store (Action (UpdateJwtUser))
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (removeItem)
import Web.HTML (window)

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
        {user} <- getStore 
        for_ user $ const $ H.modify_ _ { email = Just "test" }
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev
        { config: Config {apiBuzgibiHost}, user } <- getStore
        case user of 
          Just { token } -> do 
            resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.logout
            withError resp \(_ :: Unit) -> do 
              H.liftEffect $ window >>= localStorage >>= removeItem "buzgibi_jwt"
              H.modify_ _ { email = Nothing }
              updateStore $ UpdateJwtUser Nothing
          Nothing -> pure unit     

render { email: Just email } = 
  HH.div_ 
  [
      HH.text email
  ,   HH.form 
      [ HE.onSubmit MakeRequest] [HH.input 
      [ HPExt.type_ HPExt.InputSubmit, HPExt.value "logout"]] 
  ]
render { email: Nothing } = HH.div_ []