module Buzgibi.Component.Auth.SignUp (component, proxy) where

import Prelude

import Buzgibi.Component.Auth.SignUp.PasswordCheker (Validation, check)
import Buzgibi.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe (..), fromMaybe, isNothing)
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Data.Traversable (for)
import Data.String (length)

loc = "Buzgibi.Component.Auth.SignUp"

proxy = Proxy :: _ "auth_signUp"

data Error = Mismatch | Weak String

data Action = 
       MakeRequest Event 
     | FillEmail String 
     | FillPassword String 
     | FillRepeatedPassword String

type State = 
     { email :: Maybe String
     , password :: Maybe String
     , reapatedPassword :: Maybe String
     , strength :: Maybe Validation
     }

component =
  H.mkComponent
    { initialState: 
       const { 
          email: Nothing
        , password: Nothing
        , reapatedPassword: Nothing
        , strength: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction }
    }

handleAction (MakeRequest ev) = H.liftEffect $ preventDefault ev
handleAction (FillEmail s) = H.modify_ _ { email = Just s }
handleAction (FillPassword s) = do
  logDebug $ loc <> "pass stregth ---> " <> show (check s)
  H.modify_ _ { password = Just s, strength = Just (check s) }
handleAction (FillRepeatedPassword s) = H.modify_ _ { reapatedPassword = Just s }

render { email, password, reapatedPassword, strength } = 
  HH.div_ 
  [
      HH.form [ HE.onSubmit MakeRequest]
      [    
          HH.input 
          [ 
            HPExt.type_ HPExt.InputEmail
          , HE.onValueInput FillEmail
          ]
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