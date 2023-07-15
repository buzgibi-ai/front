module Buzgibi.Page.User.Enquiry
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Buzgibi.Data.Config
import Buzgibi.Page.Subscription.WinResize as WinResize
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Data.Route as Route
import Buzgibi.Document.Meta as Meta
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Component.List as List

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Action (WriteError))
import Store.Types (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import System.Time (getTimestamp)
import Statistics (sendComponentTime)
import Data.Int (toNumber)

proxy = Proxy :: _ "user_enquiry"

loc = "Buzgibi.Page.User.Enquiry"

data Action = 
       Initialize 
     | WinResize Int
     | Finalize
     | Submit
     | FillEnquiry String 

type State = 
     { winWidth :: Maybe Int
     , platform :: Maybe Platform
     , start :: Int
     , enquiry :: Maybe String
     }

component mkBody = 
  H.mkComponent
  { initialState: const { winWidth: Nothing, platform: Nothing, start: 0, enquiry: mempty }
    , render: render mkBody
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
  }
  where
    handleAction Initialize = do
      {user} <- getStore
      when (isNothing user) $ navigate Route.Home
      H.liftEffect $ window >>= document >>= setTitle "Buzgibi | Eqnuiry"
      { platform, config: Config {apiBuzgibiHost: host}, async } <- getStore
      w <- H.liftEffect $ window >>= innerWidth

      tm <- H.liftEffect getTimestamp

      logDebug $ loc <> " component has started at " <> show tm

      H.modify_ _ { 
          platform = pure platform
        , winWidth = pure w
        , start = tm  }

      void $ H.subscribe =<< WinResize.subscribe WinResize

      Meta.set host async $ pure $ BuzgibiBack.MetaPage (show Route.UserEnquiry)

    handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
    handleAction (FillEnquiry s) = H.modify_ _ { enquiry = Just s }
    handleAction Finalize = do
      end <- H.liftEffect getTimestamp
      {start} <- H.get
      sendComponentTime start end loc
    handleAction Submit = do 
      { config: Config {apiBuzgibiHost}, user } <- getStore
      {enquiry} <- H.get
      let req = { enquiry: fromMaybe mempty enquiry, location: { latitude: toNumber 0, longitude: toNumber 0 } }
      case user of 
        Just { token } -> do
          resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.makeEnquiry req
          withError resp \(_ :: Unit) -> do
            H.modify_ _ { enquiry = Nothing }
            logDebug $ loc <> " ---> enquiry has been sent"
        Nothing -> pure unit

render mkBody {winWidth: Just w, platform: Just p, enquiry} = 
  HH.div_ [mkBody p w (HH.div_ [searchBar enquiry, HH.div [css "list-container"] [HH.slot_ List.proxy unit List.component unit]]) ]
render _ _ = HH.div_ []

searchBar enquiry = 
 HH.form [css "search-container"]
 [
     HH.input
     [ 
         HPExt.type_ HPExt.InputText
     ,   HE.onValueInput FillEnquiry
     ,   HPExt.id "search-bar"
     ,   case enquiry of
           Just x -> HPExt.value x
           Nothing -> HPExt.placeholder "What can I help you with today?"
     ]
 ,   HH.a [HPExt.href "#", HE.onClick $ const Submit] [HH.img [css "search-icon", HPExt.src "http://www.endlessicons.com/wp-content/uploads/2012/12/search-icon.png"]]    
 ]

