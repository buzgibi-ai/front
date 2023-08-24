module Buzgibi.Page.Auth
  ( Action(..)
  , component
  , proxy_sign_in
  , proxy_sign_up
  ) where

import Prelude

import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Document.Meta as Meta
import Buzgibi.Data.Route as Route
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.HTML.Utils (css)

import Buzgibi.Data.Config
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)
import System.Time (getTimestamp)
import Statistics (sendComponentTime)
import Halogen.Store.Monad (getStore)

proxy_sign_in = Proxy :: _ "auth_container_sign_in"
proxy_sign_up = Proxy :: _ "auth_container_sign_up"

loc = "Buzgibi.Page.Auth"

data Action
  = Initialize
  | Finalize

type State =
  { start :: Int
  , route :: Route.Route
  , title :: String
  }

component authComp =
  H.mkComponent
    { initialState: \{ route, title } ->
        { start: 0
        , route: route
        , title: title
        }
    , render: const $ render authComp
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  handleAction Initialize = do
    { route, title } <- H.get
    H.liftEffect $ window >>= document >>= setTitle ("Buzgibi | " <> title)
    tm <- H.liftEffect getTimestamp
    logDebug $ loc <> " component has started at " <> show tm
    { config: Config { apiBuzgibiHost: host }, async } <- getStore
    Meta.set host async $ pure $ BuzgibiBack.MetaPage (show route)
  handleAction Finalize = do
    end <- H.liftEffect getTimestamp
    { start } <- H.get
    sendComponentTime start end loc
render comp =
 HH.main_
  [ 
      HH.div [ css "split left" ]
      [ comp
      ]
  ,   HH.div [css "split right"]
      [
          HH.div [css "side"] []
      ,   HH.img [HPExt.src "images/side-img.png"]
      ]
  ]