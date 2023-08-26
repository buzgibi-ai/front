module Buzgibi.Component.Auth.SignIn
  ( component
  , proxy
  , slot
  ) where

import Prelude

import Buzgibi.Data.Route as Route
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request.Handler (onFailure)
import Buzgibi.Data.Config (Config(..))
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Component.HTML.Utils (css, safeHref)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Halogen.Store.Monad (getStore, updateStore)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.String (length)
import Effect.Exception (message)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Web.HTML (window)
import Store (Action(UpdateJwtUser))
import Crypto.Jwt as Jwt

import Undefined

proxy = Proxy :: _ "auth_signIn"

loc = "Buzgibi.Component.Auth.SignIn"

slot = HH.slot_ proxy unit component unit

data Action
  = MakeRequest Event
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
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    when (isJust user) $ navigate $ Route.UserHistory Route.defUserHistoryParam
  handleAction (FillEmail s) = H.modify_ _ { email = Just s }
  handleAction (FillPassword s) = H.modify_ _ { password = Just s }
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    s@{ email, password } <- H.get
    logDebug $ loc <> " state ---> " <> show s
    { config: Config { apiBuzgibiHost } } <- getStore
    let
      mkCred (Just email) (Just password)
        | length password > 0 =
            Just { email: email, password: password }
        | otherwise = Nothing
      mkCred _ _ = Nothing
    let cred = mkCred email password
    case cred of
      Nothing -> do
        H.modify_ _ { errMsg = Just "login or password is empty" }
        logDebug $ loc <> "login or password is empty"
      Just cred -> do
        resp <- Request.make apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.login cred
        let
          onError e =
            H.modify_ _
              { errMsg = Just $ message e
              , email = Nothing
              , password = Nothing
              }
        onFailure resp onError \{ success: token :: String } -> do
          logDebug $ loc <> " jwt ---> " <> token
          H.liftEffect $ window >>= localStorage >>= setItem "buzgibi_jwt" token
          user <- H.liftEffect $ Jwt.parse token
          updateStore $ UpdateJwtUser (Just { jwtUser: user, token: BuzgibiBack.JWTToken token })
          navigate Route.Home

render { email, password, errMsg } =
  HH.main_
    [ HH.div [ css "screen-container" ]
        [ HH.div [ css "verticallycenter" ]
            [ HH.div [ css "split left" ]
                [ HH.div [ css "form-container" ]
                    [ HH.h1_ [ HH.text "Sign In" ]
                    , HH.h2_ [ HH.text "Login to your account with your e-mail" ]
                    , if isNothing errMsg then HH.div_ []
                      else HH.div [ HPExt.style "margin-bottom: 10px;" ] [ HH.span [ HPExt.style "color: red" ] [ HH.text (fromMaybe undefined errMsg) ] ]
                    , HH.form [ HE.onSubmit MakeRequest ]
                        [ HH.label [ HPExt.for "label" ] [ HH.text "E-mail" ]
                        , HH.input
                            [ css "form-style"
                            , HPExt.type_ HPExt.InputEmail
                            , HE.onValueInput FillEmail
                            , HPExt.value $ fromMaybe mempty email
                            , HPExt.placeholder "email"
                            ]
                        , HH.label [ HPExt.for "label" ] [ HH.text "Password" ]
                        , HH.input
                            [ css "form-style"
                            , HPExt.type_ HPExt.InputPassword
                            , HE.onValueInput FillPassword
                            , HPExt.value $ fromMaybe mempty password
                            , HPExt.placeholder "password"
                            ]
                        , HH.div [ css "CTA-container" ]
                            [ HH.div [ css "cta-button" ]
                                [ HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "submit", css "cta-button" ] ]
                            ]
                        , HH.h4 [ css "Forget" ]
                            [ HH.text "I forgot my password "
                            , HH.a [ css "link", safeHref Route.PasswordResetLink ] [ HH.text "Reset my password" ]
                            ]
                        , HH.h4 [ css "Already" ]
                            [ HH.text "Not a Buzgibi user? "
                            , HH.a [ css "link", safeHref Route.SignUp ] [ HH.text "Sign Up" ]
                            ]
                        ]
                    ]
                ]
            , HH.div [ css "split right" ]
                [ HH.div [ css "left-container" ]
                    [ HH.div [ css "image-container" ] [ HH.img [ HPExt.src "images/side-img.png" ] ]
                    ]
                ]
            ]
        ]
    ]