module Buzgibi.Component.Auth.SignUp
  ( component
  , proxy
  , slot
  )
  where

import Prelude

import Buzgibi.Component.Auth.SignUp.PasswordCheker (Validation, check)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack 
import Buzgibi.Api.Foreign.Request.Handler (onFailure) 
import Buzgibi.Data.Config (Config (..))
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route
import Buzgibi.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (..), fromMaybe, isNothing, isJust)
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Data.Traversable (for)
import Data.String (length)
import Halogen.Store.Monad (getStore, updateStore)
import Effect.Exception (message)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Web.HTML (window)
import Store (Action (UpdateJwtUser))
import Crypto.Jwt as Jwt
import Safe.Coerce


import Undefined

loc = "Buzgibi.Component.Auth.SignUp"

proxy = Proxy :: _ "auth_signUp"

data Error = Mismatch | Weak String

slot = HH.slot_ proxy unit component unit

data Action = 
       MakeRequest Event 
     | FillEmail String 
     | FillPassword String 
     | FillRepeatedPassword String
     | Initialize

type State = 
     { email :: Maybe String
     , password :: Maybe String
     , reapatedPassword :: Maybe String
     , strength :: Maybe Validation
     , errMsg :: Maybe String
     }

def = 
  { email: Nothing
  , password: Nothing
  , reapatedPassword: Nothing
  , strength: Nothing
  , errMsg: Nothing }

component =
  H.mkComponent
    { initialState: const def
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
  where
    handleAction Initialize = do 
      {user} <- getStore
      when (isJust user) $ navigate Route.Home
    handleAction (MakeRequest ev) = do 
      H.liftEffect $ preventDefault ev
      logDebug $ loc <> "pass stregth ---> trying registering"
      {email, password, reapatedPassword} <- H.get
      if password /= reapatedPassword
      then do 
        Async.send $ Async.mkOrdinary "passwords mismatch" Async.Info Nothing
        logDebug $ loc <> "passwords mismatch"
      else do 
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
            resp <- Request.make apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.register cred
            let onError e = 
                  H.modify_ _ { 
                     errMsg = Just $ message e
                  ,  email = Nothing
                  ,  password = Nothing
                  ,  reapatedPassword = Nothing
                  ,  strength = Nothing}
            onFailure resp onError \(token :: String) -> do 
              logDebug $ loc <> " jwt ---> " <> token
              H.liftEffect $ window >>= localStorage >>= setItem "buzgibi_jwt" token
              user <- H.liftEffect $ Jwt.parse $ coerce token
              updateStore $ UpdateJwtUser (Just {jwtUser: user, token: BuzgibiBack.JWTToken token })
              navigate Route.Home
    handleAction (FillEmail s) = H.modify_ _ { email = Just s }
    handleAction (FillPassword s) = do
      logDebug $ loc <> "pass stregth ---> " <> show (check s)
      H.modify_ _ { password = Just s, strength = Just (check s) }
    handleAction (FillRepeatedPassword s) = H.modify_ _ { reapatedPassword = Just s }

render { email, password, reapatedPassword, strength, errMsg} = 
  HH.div_ 
  [
      HH.form [ HE.onSubmit MakeRequest]
      [    
          HH.input 
          [ 
            HPExt.type_ HPExt.InputEmail
          , HE.onValueInput FillEmail
          ]
      ,   if isNothing errMsg 
          then HH.div_ []
          else HH.div_ [HH.text (fromMaybe undefined errMsg)]
      ,
          HH.input 
          [ HPExt.type_ HPExt.InputPassword
          , HE.onValueInput FillPassword
          ]
      ,   if isNothing password || 
             (map length password == Just 0)
          then HH.div_ [] 
          else HH.div_ $ [HH.text (fromMaybe mempty (map _.value strength))]
      ,   
          HH.input 
          [ HPExt.type_ HPExt.InputPassword
          , HE.onValueInput FillRepeatedPassword
          ]
      ,   HH.input [ HPExt.type_ HPExt.InputSubmit]
      ]
  ]