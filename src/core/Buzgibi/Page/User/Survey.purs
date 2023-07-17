module Buzgibi.Page.User.Survey
  ( Action(..)
  , component
  , proxy
  ) where

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
import Buzgibi.Component.Subscription.Logout as Logout

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store.Types (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import System.Time (getTimestamp)
import Statistics (sendComponentTime)
import Data.Int (toNumber)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.Event.Event (preventDefault)
import Data.String (length)

proxy = Proxy :: _ "user_enquiry"

loc = "Buzgibi.Page.User.Enquiry"

data Action
  = Initialize
  | WinResize Int
  | Finalize
  | Submit MouseEvent
  | FillEnquiry String
  | ToHome

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , start :: Int
  , enquiry :: Maybe String
  }

component mkBody =
  H.mkComponent
    { initialState: const
        { winWidth: Nothing
        , platform: Nothing
        , start: 0
        , enquiry: mempty
        }
    , render: render mkBody
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        , finalize = pure Finalize
        }
    }
  where
  handleAction Initialize = do
    { user } <- getStore
    when (isNothing user) $ navigate Route.Home
    H.liftEffect $ window >>= document >>= setTitle "Buzgibi | Enquiry"
    { platform, config: Config { apiBuzgibiHost: host }, async } <- getStore
    w <- H.liftEffect $ window >>= innerWidth

    tm <- H.liftEffect getTimestamp

    logDebug $ loc <> " component has started at " <> show tm

    H.modify_ _
      { platform = pure platform
      , winWidth = pure w
      , start = tm
      }

    void $ H.subscribe =<< WinResize.subscribe WinResize

    Meta.set host async $ pure $ BuzgibiBack.MetaPage (show Route.UserSurvey)

    Logout.subscribe loc $ handleAction ToHome

  handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
  handleAction (FillEnquiry s) = H.modify_ _ { enquiry = Just s }
  handleAction Finalize = do
    end <- H.liftEffect getTimestamp
    { start } <- H.get
    sendComponentTime start end loc
  handleAction (Submit ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    { enquiry } <- H.get
    query enquiry
  handleAction ToHome = navigate Route.Home

query Nothing = pure unit
query (Just enquiry) | length enquiry == 0 =
  logDebug $ loc <> " ---> an empty enquiry has been detected"
query (Just enquiry) = do
  { config: Config { apiBuzgibiHost }, user } <- getStore
  let req = { enquiry: enquiry, location: { latitude: toNumber 0, longitude: toNumber 0 } }
  case user of
    Just { token } -> do
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.makeEnquiry req
      withError resp \(_ :: Unit) -> do
        H.modify_ _ { enquiry = Nothing }
        logDebug $ loc <> " ---> enquiry has been sent"
    Nothing -> pure unit

render mkBody { winWidth: Just w, platform: Just p, enquiry } =
  HH.div_ [ mkBody p w (searchBar enquiry) ]
render _ _ = HH.div_ []

searchBar enquiry =
  HH.form [ css "search-container" ]
    [ HH.input
        [ HPExt.type_ HPExt.InputText
        , HE.onValueInput FillEnquiry
        , HPExt.id "search-bar"
        , case enquiry of
            Just x -> HPExt.value x
            Nothing -> HPExt.placeholder "What can I help you with today?"
        ]
    , HH.a [ HPExt.href "#", HE.onClick Submit ] [ HH.img [ css "search-icon", HPExt.src "http://www.endlessicons.com/wp-content/uploads/2012/12/search-icon.png" ] ]
    ]
