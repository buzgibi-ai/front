module Buzgibi.Component.Auth.Email
  ( Action
  , component
  , proxy
  ) where

import Prelude

import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config (Config(..))
import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Halogen.Store.Monad (getStore)
import Data.Traversable (for_)

proxy = Proxy :: _ "auth_email_confirmation"

type State = { isConfirmed :: Maybe Boolean, key :: String }

data Action = Initialize

component =
  H.mkComponent
    { initialState: \{ key } -> { isConfirmed: Nothing, key: key }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { config: Config { apiBuzgibiHost }, user } <- getStore
    for_ user \{ token } -> do
      { key } <- H.get
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkAuthApi $ BuzgibiBack.confirmEmail { key: key }
      withError resp \{ success: res } -> H.modify_ _ { isConfirmed = Just res }

render { isConfirmed } =
  HH.main_
    [ HH.div [ css "screen-container" ]
        [ HH.div [ css "verticallycenter" ]
            [ HH.div [ css "split left" ]
                [ HH.div [ css "form-container" ] $
                    case isConfirmed of
                      Nothing -> [ HH.text "confirmation is being processed..." ]
                      Just true -> [ HH.text "the email is confirmed" ]
                      Just false -> [ HH.text "we cannot confirm the email" ]
                ]
            , HH.div [ css "split right" ]
                [ HH.div [ css "left-container" ]
                    [ HH.div [ css "image-container" ] [ HH.img [ HPExt.src "images/side-img.png" ] ]
                    ]
                ]
            ]
        ]
    ]