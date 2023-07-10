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
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Capability.LogMessages (logDebug)

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
import Effect.AVar (tryPut) as Async

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
        for_ user \{jwtUser: {email}} -> H.modify_ _ { email = Just email }
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev
        { config: Config {apiBuzgibiHost}, user, isLogoutVar } <- getStore
        case user of 
          Just { token } -> do 
            resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.logout
            withError resp \(_ :: Unit) -> do 
              H.liftEffect $ window >>= localStorage >>= removeItem "buzgibi_jwt"
              H.modify_ _ { email = Nothing }
              updateStore $ UpdateJwtUser Nothing
              res <- H.liftEffect $ Async.tryPut unit isLogoutVar
              logDebug $ loc <> " if avar operaion succeeded ---> " <> show res
          Nothing -> pure unit

render { email: Just email } = 
  HH.form 
  [  HE.onSubmit MakeRequest
  ,  css "form-inline"
  ] 
  [ HH.text email
  , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "Sign out"]
  ] 
render { email: Nothing } = HH.div_ []