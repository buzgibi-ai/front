module Buzgibi.Component.Auth.Password.NewPassword
  ( Action(..)
  , component
  , proxy
  , slot
  ) where

import Prelude

import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Data.Config (Config(..))
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.Extended as HPExt
import Data.String (length)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault, Event)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Store.Monad (getStore)
import Data.Foldable (for_)

proxy = Proxy :: _ "auth_password_new"

type State = { key :: String, pass :: Maybe String, repeatPass :: Maybe String, result :: Maybe Boolean }

data Action = MakeRequest Event | FillPassword Int String

type Key = { key :: String }

slot key = HH.slot_ proxy unit component { key: key }

component =
  H.mkComponent
    { initialState: \{ key } -> { key: key, pass: Nothing, repeatPass: Nothing, result: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction }
    }
  where
  handleAction (FillPassword 1 v) = do
    { pass } <- H.get
    H.modify_ _ { pass = if length v == 0 then pass else Just v }
  handleAction (FillPassword 2 v) = do
    { repeatPass } <- H.get
    H.modify_ _ { repeatPass = if length v == 0 then repeatPass else Just v }
  handleAction (FillPassword _ _) = pure unit
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    { pass, repeatPass, key } <- H.get
    when (pass == repeatPass)
      $ for_ pass
      $ \pass -> do
          { config: Config { apiBuzgibiHost } } <- getStore
          let body = { password: pass, key: key }
          res <- Request.make apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.setNewPassword body
          withError res \{ success: res } ->
            H.modify_ _ { result = Just res, pass = Nothing, repeatPass = Nothing }

render { pass, repeatPass, result } =
  HH.main_
    [ HH.div [ css "screen-container" ]
        [ HH.div [ css "verticallycenter" ]
            [ HH.div [ css "split left" ]
                [ HH.div [ css "form-container" ]
                    [ HH.h1_ [ HH.text "New Password" ]
                    , HH.h2_ [ HH.text "Set a new password to your account" ]
                    , case result of
                        Just false -> HH.text "key is either broken or exipred"
                        Just true -> HH.text "the password has been changed successfully"
                        Nothing -> HH.text mempty
                    , HH.form
                        [ HE.onSubmit MakeRequest ]
                        [ if pass /= repeatPass then HH.text "passwords mismatch" else HH.text mempty
                        , HH.label [ HPExt.for "label" ] [ HH.text "Password" ]
                        , HH.input
                            [ HPExt.type_ HPExt.InputText
                            , HE.onValueInput $ FillPassword 1
                            , HPExt.value $ fromMaybe mempty pass
                            , HPExt.placeholder "password"
                            ]
                        , HH.label [ HPExt.for "label" ] [ HH.text "New password" ]
                        , HH.input
                            [ HPExt.type_ HPExt.InputText
                            , HE.onValueInput $ FillPassword 2
                            , HPExt.value $ fromMaybe mempty repeatPass
                            , HPExt.placeholder "repeat password"
                            ]
                        , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "submit" ]
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
