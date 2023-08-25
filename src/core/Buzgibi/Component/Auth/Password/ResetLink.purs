module Buzgibi.Component.Auth.Password.ResetLink
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
import Foreign (isNull, unsafeFromForeign)
import Data.Foldable (for_)

import Undefined (undefined)

proxy = Proxy :: _ "auth_password_link"

type State = { email :: Maybe String, result :: Maybe Int, isSuccess :: Boolean }

data Action = MakeRequest Event | FillEmail String

slot = HH.slot_ proxy unit component unit

component =
  H.mkComponent
    { initialState: const { email: Nothing, result: Nothing :: Maybe Int, isSuccess: false }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction }
    }
  where
  handleAction (FillEmail v) = do
    { email } <- H.get
    H.modify_ _ { email = if length v == 0 then email else Just v }
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    { email } <- H.get
    { config: Config { apiBuzgibiHost } } <- getStore
    for_ email \e -> do
      res <- Request.make apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.sendResetPasswordLink e
      withError res \{ success: o } ->
        if isNull o then H.modify_ _ { email = Nothing, isSuccess = true }
        else H.modify_ _ { email = Nothing, result = Just $ unsafeFromForeign o }

render { email, result: Nothing, isSuccess } =
  HH.div_
    [ if isSuccess then HH.text "reset link has been sent to the requeted email" else HH.text mempty
    , HH.form [ HE.onSubmit MakeRequest ]
        [ HH.input
            [ HPExt.type_ HPExt.InputText
            , HE.onValueInput FillEmail
            , HPExt.value $ fromMaybe mempty email
            , HPExt.placeholder "email"
            ]
        , HH.input [ HPExt.type_ HPExt.InputSubmit, HPExt.value "submit" ]
        ]
    ]
render { result: Just left } = HH.div_ [ HH.text ("the next try will be available in " <> show left <> " sec") ]