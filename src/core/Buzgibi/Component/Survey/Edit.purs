module Buzgibi.Component.Survey.Edit
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Subscription.Logout as Logout
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Page.Subscription.WinResize as WinResize
import Buzgibi.Component.Utils (with404, withAuth)
import Buzgibi.Api.Foreign.Request.Handler (withAffjax, withError)
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Data.Config
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Store.Types (Platform)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import System.Time (getTimestamp)
import Statistics (sendComponentTime)
import Effect.AVar as Async
import Web.File.Blob (fromString)
import Web.File.Url (createObjectURL)
import Data.MediaType (MediaType (..))
import Data.Base64 as Base64
import Affjax.Web as AX
import Affjax.ResponseFormat as AX
import Affjax.RequestBody as AXB
import Data.String (null)
import Web.Event.Event (preventDefault, Event)
import Data.Traversable (for_)

import Undefined

proxy = Proxy :: _ "survey_edit"

loc = "Buzgibi.Component.Survey.Edit"

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , start :: Int
  , voice :: String
  , surveyIdent :: Int
  , survey :: Maybe String
  }

data Action
  = Initialize
  | WinResize Int
  | Finalize
  | ToHome
  | MakeRequest Event
  | SetSurvey String

component mkBody =
  H.mkComponent
  { initialState: const
    { winWidth: Nothing
    , platform: Nothing
    , start: 0
    , voice: mempty
    , survey: Nothing
    , surveyIdent: 0
    }
  , render: render mkBody
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = pure Initialize
    , finalize = pure Finalize
    }
  }
  where
    handleAction ToHome = navigate Route.Home
    handleAction Initialize = do 
      { user, editSurvey, config: Config { apiBuzgibiHost } } <- getStore
      editm <- H.liftEffect $ Async.tryTake editSurvey
      withAuth user $ \{ jwtUser: { ident: userId } } ->
        with404 editm \{voice: voiceIdent, survey: ident} -> do
          H.liftEffect $ window >>= document >>= setTitle "Buzgibi | Survey Edit"
          { platform } <- getStore
          w <- H.liftEffect $ window >>= innerWidth

          tm <- H.liftEffect getTimestamp

          logDebug $ loc <> " component has started at " <> show tm
          resp <- H.liftAff $ AX.get AX.blob $ apiBuzgibiHost <> "/file/download/" <> show userId <> "/raw/" <> show voiceIdent
          
          withAffjax loc resp \blob -> do
            voiceUrl <- H.liftEffect $ createObjectURL blob
            H.modify_ _
              { platform = pure platform
              , winWidth = pure w
              , start = tm
              , voice = voiceUrl
              , surveyIdent = ident
              }

          void $ H.subscribe =<< WinResize.subscribe WinResize
          Logout.subscribe loc $ handleAction ToHome

    handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
    handleAction Finalize = do
      end <- H.liftEffect getTimestamp
      { start } <- H.get
      sendComponentTime start end loc
    handleAction (MakeRequest ev) = do
      H.liftEffect $ preventDefault ev
      {survey, surveyIdent} <- H.get
      logDebug $ loc <> " ---> edit: " <> show survey
      for_ survey \val -> do
        { config: Config { apiBuzgibiHost }, user } <- getStore
        for_ user \{ token, jwtUser: {ident} } -> do
          resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ 
                    BuzgibiBack.editSurvey surveyIdent { survey: val }
          withError resp \{success: ifOk} ->
            if ifOk then
              Async.send $ Async.mkOrdinary "survey has been changed" Async.Success Nothing
            else 
              Async.send $ Async.mkOrdinary "changes hasn't been applied" Async.Warning Nothing
          H.modify_ _ { survey = Nothing }     
    handleAction (SetSurvey val) = do 
      s <- H.get
      H.modify_ _ { survey = if null val then Nothing else Just val }   

render mkBody { winWidth: Just w, platform: Just p, voice, survey } = mkBody p w $ 
  HH.form [ css "search-container", HE.onSubmit MakeRequest ]
  [
      HH.div_ [HH.audio [HP.controls true] [HH.source [HP.src voice, HP.type_ (MediaType "audio/wav")]]] 
  ,   HH.input
      [ HP.type_ HP.InputText
      , HE.onValueInput SetSurvey
      , HP.value $ fromMaybe mempty survey
      ]
  ,  HH.input [ css "form-control", HP.type_ HP.InputSubmit, HP.value "submit" ]
  ]
render _ _ = HH.div_ []