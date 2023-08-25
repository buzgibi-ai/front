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

render { pass, repeatPass, result: Nothing } =
  HH.form [ HE.onSubmit MakeRequest ]
    [ if pass /= repeatPass then HH.text "passwords mismatch" else HH.text mempty
    , HH.input
        [ HPExt.type_ HPExt.InputText
        , HE.onValueInput $ FillPassword 1
        , HPExt.value $ fromMaybe mempty pass
        , HPExt.placeholder "password"
        ]
    , HH.input
        [ HPExt.type_ HPExt.InputText
        , HE.onValueInput $ FillPassword 2
        , HPExt.value $ fromMaybe mempty repeatPass
        , HPExt.placeholder "repeat password"
        ]
    , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "submit" ]
    ]
render { result: Just false } = HH.div_ [ HH.text "key is either broken or exipred" ]
render { result: Just true } = HH.div_ [ HH.text "the password has been changed successfully" ]
