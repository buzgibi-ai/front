module Buzgibi.Page.HomeV2 (component, proxy) where

import Prelude

import Buzgibi.Page.Home.HtmlV2 (html)
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Data.Maybe (isJust)
import Halogen.Store.Monad (getStore)

proxy = Proxy :: _ "home_v2"

loc = "Buzgibi.Page.HomeV2"

data Action = Initialize

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    when (isJust user) $ navigate Route.UserHome

render = html