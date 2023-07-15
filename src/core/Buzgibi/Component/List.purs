module Buzgibi.Component.List
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Component.Async (withAffjax)
import Buzgibi.Component.Pagination as Pagination 

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe (..))
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Affjax.Web as AX
import Affjax.ResponseFormat as AX
import Affjax.RequestBody as AXB
import File.Blob (downloadBlob)
import Data.Array (snoc)

proxy = Proxy :: _ "list"

loc = "Buzgibi.Component.List"

type State = { list :: Array BuzgibiBack.HistoryItem, isNext :: Boolean }

data Action = Initialize | Download Int String Event

component = 
  H.mkComponent
  { initialState: const { list: [], isNext: false }
    , render: render
    , eval: H.mkEval H.defaultEval
      {  handleAction = handleAction
      , initialize = pure Initialize
      }
  }
  where 
    handleAction Initialize = do 
      { user, config: Config {apiBuzgibiHost} } <- getStore
      case user of 
        Just { token } -> do
          resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.getHistory Nothing
          withError resp \{ items, isnextpage } ->  H.modify_ _ { list = items, isNext = isnextpage }
        Nothing -> pure unit
    handleAction (Download ident name ev) = do
      H.liftEffect $ preventDefault ev
      logDebug $ loc <> " ---> downloaded file " <> name
      { config: Config {apiBuzgibiHost}, async, user } <- getStore
      case user of
        Just { jwtUser: {ident: userId} } -> 
          H.liftAff $ do
            resp <- AX.get AX.blob $ apiBuzgibiHost <> "/file/download/" <> show userId <> "/raw/" <> show ident
            withAffjax loc async resp $ pure <<< flip downloadBlob name
        Nothing -> pure unit

render { list: [] } = HH.text "you haven't the history to be shown" 
render { list, isNext } = 
  HH.div [css "history-item-container"] $
  (list <#> \{ident, name, timestamp} -> 
      HH.div [HPExt.style "margin-top: 10px"] 
      [
          HH.form [ HE.onSubmit $ Download ident name]
          [ 
              HH.input 
              [ 
                  HPExt.style "cursor: pointer"
              ,   HPExt.type_ HPExt.InputSubmit
              ,   HPExt.value name
              ]
          ]
      ]
  ) `snoc` HH.div [HPExt.style "margin-top: 10px"] [HH.slot_ Pagination.proxy unit Pagination.component { currenPage: 1, isNext: isNext }]