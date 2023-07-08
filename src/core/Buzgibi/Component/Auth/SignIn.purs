module Buzgibi.Component.Auth.SignIn
  ( component
  , proxy
  , slot
  )
  where

import Prelude

import Buzgibi.Data.Route as Route
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request.Handler (onFailure)
import Buzgibi.Data.Config (Config (..))
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route
import Buzgibi.Component.Async as Async
import Buzgibi.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.Store.Monad (getStore, updateStore)
import Data.Maybe (Maybe (..), isJust, isNothing, fromMaybe)
import Data.String (length)
import Effect.Exception (message)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Web.HTML (window)
import Store (Action (UpdateJwtUser))
import Crypto.Jwt as Jwt

import Undefined

proxy = Proxy :: _ "auth_signIn"

loc = "Buzgibi.Component.Auth.SignIn"

slot = HH.slot_ proxy unit component unit

data Action = 
       MakeRequest Event 
     | Initialize
     | FillEmail String 
     | FillPassword String 

type State = 
     { email :: Maybe String
     , password :: Maybe String
     , errMsg :: Maybe String
     }

component =
  H.mkComponent
    { initialState: const { email: Nothing, password: Nothing, errMsg: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
    where 
      handleAction Initialize = do
        {user} <- getStore
        when (isJust user) $ navigate Route.Home
      handleAction (FillEmail s) = H.modify_ _ { email = Just s }
      handleAction (FillPassword s) = H.modify_ _ { password = Just s }        
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev
        s@{email, password} <- H.get
        logDebug $ loc <> " state ---> " <> show s
        { config: Config {apiBuzgibiHost} } <- getStore
        let mkCred (Just email) (Just password) 
              | length password > 0 =
                Just { email: email, password: password }
              | otherwise = Nothing  
            mkCred _ _ = Nothing 
        let cred = mkCred email password
        case cred of
          Nothing -> do
            Async.send $ Async.mkOrdinary "login or password is empty" Async.Info Nothing
            logDebug $ loc <> "login or password is empty"
          Just cred -> do
            resp <- Request.make apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.login cred
            let onError e = 
                  H.modify_ _ { 
                     errMsg = Just $ message e
                  ,  email = Nothing
                  ,  password = Nothing}
            onFailure resp onError \(token :: String) -> do 
              logDebug $ loc <> " jwt ---> " <> token
              H.liftEffect $ window >>= localStorage >>= setItem "buzgibi_jwt" token
              user <- H.liftEffect $ Jwt.parse token
              updateStore $ UpdateJwtUser (Just {jwtUser: user, token: BuzgibiBack.JWTToken token })
              navigate Route.Home

render { email, password, errMsg} = 
  HH.div_ 
  [
      if isNothing errMsg 
      then HH.div_ []
      else HH.div_ [HH.text (fromMaybe undefined errMsg)]
  ,        
      HH.form [ HE.onSubmit MakeRequest]
      [    
          HH.input 
          [ 
            HPExt.type_ HPExt.InputEmail
          , HE.onValueInput FillEmail
          , HPExt.value $ fromMaybe mempty email
          ]
      ,
          HH.input 
          [ HPExt.type_ HPExt.InputPassword
          , HE.onValueInput FillPassword
          , HPExt.value $ fromMaybe mempty password
          ]
      ,   HH.input [ HPExt.type_ HPExt.InputSubmit]
      ]
  ]