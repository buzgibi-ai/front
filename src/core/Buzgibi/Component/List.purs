module Buzgibi.Component.List
  ( Action(..)
  , component
  , proxy
  ) where

import Prelude

import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Api.Foreign.Request as Request
import Buzgibi.Api.Foreign.Request.Handler (withError)
import Buzgibi.Data.Config
import Buzgibi.Component.HTML.Utils (css, safeHref, maybeElem)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Component.Async (withAffjax)
import Buzgibi.Component.Pagination as Pagination
import Buzgibi.Component.Subscription.Pagination as Pagination
import Buzgibi.Component.Async as Async
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Data.Route (Route (UserSurvey))
import Buzgibi.Capability.Navigate (navigate)
import Buzgibi.Data.Route as Route

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties.Extended as HPExt
import Halogen.HTML.Events as HE
import Web.Event.Event (preventDefault, Event)
import Affjax.Web as AX
import Affjax.ResponseFormat as AX
import Affjax.RequestBody as AXB
import File.Blob (downloadBlob)
import Data.Array (snoc)
import Data.String (length, take, joinWith)
import Effect.AVar as Async
import DOM.HTML.Indexed.ScopeValue (ScopeValue(ScopeCol))
import Foreign (isUndefined, unsafeFromForeign)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Undefined
import Data.Array ((:))
import DOM.HTML.Indexed.ButtonType (ButtonType (ButtonButton))
import Data.Foldable (for_)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Foreign (Foreign)

proxy = Proxy :: _ "list"

loc = "Buzgibi.Component.List"

type State =
  { list :: Array BuzgibiBack.WithFieldStatusHistoryItem
  , total :: Int
  , perpage :: Int
  , hash :: String
  , constants :: Map.Map String String
  , currPage :: Int
  }

data Action = 
     Initialize | 
     Download Int String Event | 
     Query Int | 
     LangChange String (Map.Map String String) | 
     Submit Int Boolean MouseEvent |
     Edit Int Foreign MouseEvent

component =
  H.mkComponent
    { initialState: \{page} -> 
      { list: [], 
        total: 0, 
        perpage: 0, 
        hash: mempty :: String, 
        constants: 
        Map.empty, 
        currPage: page
      }
    , render: render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = pure Initialize
        }
    }
  where
  getHistory page = do
    { user, config: Config { apiBuzgibiHost } } <- getStore
    case user of
      Just { token } -> do
        resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.getHistory page
        logDebug $ loc <> " ---> get history for page " <> show page
        withError resp \{ success: { items, total, perpage } } -> do 
          logDebug $ loc <> " ---> get history items " <> joinWith "," (map BuzgibiBack.printWithFieldStatusHistoryItem items) <> " for page " <> show page
          H.modify_ _ { list = items, total = total, perpage = perpage, currPage = maybe 1 _.page page  }
      Nothing -> pure unit
  handleAction Initialize = do
    void $ initTranslation loc \hash translation -> do
      let constants = 
            fromMaybe undefined $ 
              Map.lookup "history" $ 
                BuzgibiBack.getTranslationEndpoints translation
      logDebug $ loc <> " ---> conttants " <> show constants          
      H.modify_ _ { hash = hash, constants = constants }
    {currPage} <- H.get
    getHistory $ Just { page: currPage }
    Pagination.subscribe loc $ handleAction <<< Query
    Translation.subscribe loc $ \hash translation ->
      let constants = 
            fromMaybe undefined $ 
              Map.lookup "history" $ 
                BuzgibiBack.getTranslationEndpoints translation
      in handleAction $ LangChange hash constants

  handleAction (Download ident name ev) = do
    H.liftEffect $ preventDefault ev
    logDebug $ loc <> " ---> downloaded file " <> name
    { config: Config { apiBuzgibiHost }, async, user } <- getStore
    case user of
      Just { jwtUser: { ident: userId } } ->
        H.liftAff $ do
          resp <- AX.get AX.blob $ apiBuzgibiHost <> "/file/download/" <> show userId <> "/raw/" <> show ident
          withAffjax loc async resp $ pure <<< flip downloadBlob name
      Nothing -> pure unit
  handleAction (Query page) = getHistory $ Just { page: page }
  handleAction (LangChange hash xs) = H.modify_ _ { constants = xs, hash = hash }
  handleAction (Submit _ true ev) = do
   H.liftEffect $ preventDefault $ toEvent ev
   {constants} <- H.get
   let value = fromMaybe "..." $ Map.lookup "bark" constants
   Async.send $ Async.mkOrdinary value Async.Warning Nothing
  handleAction (Submit ident false ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    { config: Config { apiBuzgibiHost }, user } <- getStore
    for_ user \{ token } -> do
      resp <- Request.makeAuth (Just token) apiBuzgibiHost BuzgibiBack.mkUserApi $ BuzgibiBack.submitSurvey {ident: ident}
      withError resp $ const $ H.modify_ \s -> 
        s { list = 
              _.list s <#> \el@{surveyident} -> 
                if surveyident == ident then 
                el { status = "inProcess" } else el }
  handleAction (Edit _ voice ev) | isUndefined voice = do
    H.liftEffect $ preventDefault $ toEvent ev
    {constants} <- H.get
    let value = fromMaybe "..." $ Map.lookup "bark" constants
    Async.send $ Async.mkOrdinary value Async.Warning Nothing        
  handleAction (Edit ident voice ev) = do
    H.liftEffect $ preventDefault $ toEvent ev
    logDebug $ loc <> " ---> edit survey " <> show ident
    {editSurvey} <- getStore
    let unwrappedVoice = unsafeFromForeign voice
    void $ H.liftEffect $ { survey: ident, voice: unwrappedVoice } `Async.tryPut` editSurvey
    navigate $ Route.EditSurvey ident

render { list: [] } = HH.text "you haven't the history to be shown"
render { list, total, perpage, constants, currPage } =
  HH.div
  [ css "history-item-container" ]
  [ HH.table_
      [ HH.thead_
          [ HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "title" constants) ]
          , HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "time" constants) ]
          , HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "status" constants) ]
          , HH.th [ HPExt.scope ScopeCol ] [ HH.text $ fromMaybe "..." (Map.lookup "report" constants) ]
          ]
      , HH.tbody_
          ( list <#> \{ surveyident, reportident, name, timestamp, status, voice } ->
              HH.tr_
                [ HH.td [ HPExt.dataLabel "title" ] 
                  (HH.text (if length name > 20 then take 20 name else name) :
                  if status == "draft" then 
                    [  HH.a
                      [ css "nav-link" 
                      , safeHref (Route.EditSurvey surveyident)
                      , HE.onClick (Edit surveyident (unsafeFromForeign voice))
                      ] [HH.text "edit"]
                    , HH.a
                      [ css "nav-link"
                      , HE.onClick (Submit surveyident (isUndefined voice))
                      ] [HH.text "submit"]
                    ]
                  else [])
                , HH.td [ HPExt.dataLabel "time" ] [ HH.text timestamp ]
                , HH.td [ HPExt.dataLabel "status" ] [ HH.text $ fromMaybe "..." (Map.lookup status constants) ]
                , HH.td [ HPExt.dataLabel "report" ] $
                    if isUndefined reportident
                    then [HH.div_ [HH.text "-"]]
                    else 
                        [ HH.form [ HE.onSubmit $ Download ((unsafeFromForeign reportident) :: Int) name ]
                            [ HH.input
                                [ HPExt.style "cursor: pointer"
                                , HPExt.type_ HPExt.InputSubmit
                                , HPExt.value $ if length name > 10 then take 10 name <> "..." else name
                                ]
                            ]
                        ]
                ]
          )
      ]   
  , HH.div [ HPExt.style "margin-top: 10px" ] [ HH.slot_ Pagination.proxy unit Pagination.component { total: total, perpage: perpage, page: currPage } ]
  , HH.div_
    [
        HH.a
        [ css "nav-link"
        , safeHref UserSurvey
        ]
        [ HH.text $ fromMaybe "..." (Map.lookup "makeSurvey" constants) ]
    ] 
  ]