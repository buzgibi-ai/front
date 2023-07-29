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
import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Component.Async (withAffjax)
import Buzgibi.Component.Pagination as Pagination
import Buzgibi.Component.Subscription.Pagination as Pagination
import Buzgibi.Component.Async as Async
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Data.Route (Route (UserSurvey))

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
import Data.String (length, take)
import Effect.AVar as Async
import DOM.HTML.Indexed.ScopeValue (ScopeValue(ScopeCol))
import Foreign (isUndefined, unsafeFromForeign)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Undefined

proxy = Proxy :: _ "list"

loc = "Buzgibi.Component.List"

type State =
  { list :: Array BuzgibiBack.WithFieldStatusHistoryItem
  , total :: Int
  , perpage :: Int
  , hash :: String
  , constants :: Map.Map String String
  }

data Action = Initialize | Download Int String Event | Query Int | LangChange String (Map.Map String String)

component =
  H.mkComponent
    { initialState: const { list: [], total: 0, perpage: 0, hash: mempty :: String, constants: Map.empty }
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
        withError resp \{ success: { items, total, perpage } } ->
          H.modify_ _ { list = items, total = total, perpage = perpage }
      Nothing -> pure unit
  handleAction Initialize = do
    void $ initTranslation loc \hash translation -> do
      let constants = 
            fromMaybe undefined $ 
              Map.lookup "history" $ 
                BuzgibiBack.getTranslationEndpoints translation
      logDebug $ loc <> " ---> conttants " <> show constants          
      H.modify_ _ { hash = hash, constants = constants }
    getHistory Nothing
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

render { list: [] } = HH.text "you haven't the history to be shown"
render { list, total, perpage, constants } =
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
          ( list <#> \{ ident, name, timestamp, status } ->
              HH.tr_
                [ HH.td [ HPExt.dataLabel "title" ] [ HH.text (if length name > 20 then take 20 name else name) ]
                , HH.td [ HPExt.dataLabel "time" ] [ HH.text timestamp ]
                , HH.td [ HPExt.dataLabel "status" ] [ HH.text $ fromMaybe "..." (Map.lookup status constants) ]
                , HH.td [ HPExt.dataLabel "report" ] $
                    if isUndefined ident
                    then [HH.div_ [HH.text "-"]]
                    else 
                        [ HH.form [ HE.onSubmit $ Download ((unsafeFromForeign ident) :: Int) name ]
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
  , HH.div [ HPExt.style "margin-top: 10px" ] [ HH.slot_ Pagination.proxy unit Pagination.component { total: total, perpage: perpage } ]
  , HH.div_
    [
        HH.a
        [ css "nav-link"
        , safeHref UserSurvey
        ]
        [ HH.text $ fromMaybe "..." (Map.lookup "makeSurvey" constants) ]
    ]
  ]
