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
import Buzgibi.Data.Survey
import Buzgibi.Page.User.Survey.File as File
import Buzgibi.Component.Async as Async

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Halogen.Query.Input (RefLabel (..))
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
import Data.String (length)
import Web.File.File (File, name)
import Web.Event.Event (preventDefault, Event)
import Data.Array ((..))
import Data.Traversable (for_, traverse_)
import Data.Enum (fromEnum, toEnum)
import Undefined

proxy = Proxy :: _ "user_survey"

loc = "Buzgibi.Page.User.Survey"

data Action
  = Initialize
  | WinResize Int
  | Finalize
  | Upload (Array File)
  | ToHome
  | MakeRequest Event
  | SetCategory Int
  | SetAssessmentScore Int
  | SetSurvey String

type State =
  { winWidth :: Maybe Int
  , platform :: Maybe Platform
  , start :: Int
  , survey :: Maybe BuzgibiBack.Survey
  , isSurveyEmpty :: Boolean 
  }

component mkBody =
  H.mkComponent
    { initialState: const
        { winWidth: Nothing
        , platform: Nothing
        , start: 0
        , survey: Nothing
        , isSurveyEmpty: false
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
  handleAction Finalize = do
    end <- H.liftEffect getTimestamp
    { start } <- H.get
    sendComponentTime start end loc
  handleAction (Upload xs) = for_ xs uploadFile
  handleAction ToHome = navigate Route.Home
  handleAction (MakeRequest ev) = do
    H.liftEffect $ preventDefault ev
    {survey} <- H.get
    logDebug $ loc <> " ---> survey: " <> show survey
    for_ survey submitSurvey
  handleAction (SetCategory idx) = do
    s <- H.get
    let setCategory x = x { category = maybe undefined show (toEnum idx :: Maybe Category) }  
    H.modify_ \s -> s { survey = map setCategory (_.survey s) }
  handleAction (SetAssessmentScore idx) = do
    s <- H.get
    let setAssessmentScore x = x { assessmentscore = maybe undefined show (toEnum idx :: Maybe AssessmentScore) }  
    H.modify_ \s -> s { survey = map setAssessmentScore (_.survey s) }
  handleAction (SetSurvey val) = do 
    s <- H.get
    let setSurvey x = x { survey = val }  
    H.modify_ \s -> s { survey = map setSurvey (_.survey s), isSurveyEmpty = false }

uploadFile file = do
  { config: Config { apiBuzgibiHost }, user } <- getStore
  for_ user \{ token, jwtUser: {ident} } -> do
    resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkFileApi $ 
              BuzgibiBack.upload ("user" <> show (ident :: Int)) file
    withError resp \(ident :: Int) -> do
      let survey =
            { survey: mempty :: String
            , assessmentscore: show YN
            , category: show CustomerSatisfaction
            , phonesfileident: ident
            , location: { latitude: toNumber 0, longitude: toNumber 0 }
            }
      H.modify_ _ { survey = pure survey }
      logDebug $ loc <> " ---> file has been upload, id " <> show ident

submitSurvey survey = do
  { config: Config { apiBuzgibiHost }, user } <- getStore
  for_ user \{ token, jwtUser: {ident} } -> do
    if (((<) 0) <<< length <<< _.survey) survey == true
    then do  
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ 
                BuzgibiBack.makeSurvey survey
      withError resp \(ident :: Unit) -> do
        H.getRef (RefLabel "file") >>= traverse_ (H.liftEffect <<< File.removeValue)
        H.modify_ _ { survey = Nothing, isSurveyEmpty = false }
        logDebug $ loc <> " ---> survey has been handed over"
        Async.send $ Async.mkOrdinary "survey has been submitted" Async.Success Nothing
    else H.modify_ _ { isSurveyEmpty = true }

render mkBody { winWidth: Just w, platform: Just p, survey, isSurveyEmpty } =
  HH.div_ [ mkBody p w (surveyForm survey isSurveyEmpty) ]
render _ _ = HH.div_ []

surveyForm survey isSurveyEmpty =
  HH.form [ css "search-container", HE.onSubmit MakeRequest ]
  [
      HH.div [css "form-group"]
      [ 
          HH.input
          [ HPExt.type_ HPExt.InputFile
          , HE.onFileUpload Upload
          , css "form-control"
          , HPExt.ref $ RefLabel "file"
          ]
      ,   HH.select [ css "form-control", HE.onSelectedIndexChange SetCategory] $ 
            (fromEnum CustomerSatisfaction .. fromEnum PoliticalPoll) <#> \x ->
              HH.option_ [HH.text (show (fromMaybe undefined (toEnum x :: Maybe Category)))]
      ,   HH.select [ css "form-control", HE.onSelectedIndexChange SetAssessmentScore] $ 
            (fromEnum YN .. fromEnum ScaleOf10) <#> \x ->
              HH.option_ [HH.text (show (fromMaybe undefined (toEnum x :: Maybe AssessmentScore)))]
      ,   HH.input
          [ HPExt.type_ HPExt.InputText
          , css $ "form-control " <> if isSurveyEmpty then "border border-danger" else mempty
          , HE.onValueInput SetSurvey
          , HPExt.value $ maybe mempty (_.survey) survey
          ]
      ,   HH.input [ css "form-control", HPExt.type_ HPExt.InputSubmit, HPExt.value "submit" ]    
      ]
  ]