module Buzgibi.Page.Home
  ( Action(..)
  , component
  , proxy
  )
  where

import Prelude

import Buzgibi.Page.Subscription.WinResize as WinResize 
import Buzgibi.Api.Foreign.BuzgibiBack as BuzgibiBack
import Buzgibi.Capability.LogMessages (logDebug)
import Buzgibi.Data.Route as Route
import Buzgibi.Document.Meta as Meta
import Buzgibi.Component.Utils (initTranslation)
import Buzgibi.Component.Subscription.Translation as Translation
import Buzgibi.Data.Config

import Halogen as H
import Halogen.HTML as HH
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, innerWidth)
import Web.HTML (window)
import Type.Proxy (Proxy(..))
import Store (Action (WriteError))
import Store.Types (Platform)
import Data.Maybe
import Halogen.Store.Monad (getStore)
import Data.Map as Map
import System.Time (getTimestamp)
import Statistics (sendComponentTime) 
import Halogen.Html.Raw.Render as H

import Undefined

proxy = Proxy :: _ "home"

loc = "Buzgibi.Page.Home"

data Action = 
       Initialize 
     | WinResize Int 
     | LangChange String (Map.Map String String)
     | Finalize

type State = 
     { winWidth :: Maybe Int
     , platform :: Maybe Platform
     , body :: Maybe String
     , hash :: String
     , start :: Int
     }

component mkBody =
  H.mkComponent
    { initialState: const 
      { winWidth: Nothing
      , platform: Nothing
      , body: Nothing
      , hash: mempty
      , start: 0 }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , finalize = pure Finalize
      }
    }
    where 
      render {winWidth: Just w, platform: Just p, body: body} = 
        HH.div_ [mkBody p w (content body) ]
      render _ = HH.div_ []
      handleAction Initialize = do
        H.liftEffect $ window >>= document >>= setTitle "Buzgibi | Home"
        { platform, config: Config {apiBuzgibiHost: host}, async } <- getStore
        w <- H.liftEffect $ window >>= innerWidth

        tm <- H.liftEffect getTimestamp

        logDebug $ loc <> " component has started at " <> show tm

        void $ initTranslation loc \hash translation ->
          H.modify_ _ { 
              platform = pure platform
           ,  winWidth = pure w
           , body = Map.lookup "home" $ BuzgibiBack.getTranslationPage translation
           , hash = hash
           , start = tm  }

        void $ H.subscribe =<< WinResize.subscribe WinResize

        Meta.set host async $ pure $ BuzgibiBack.MetaPage (show Route.Home)

        Translation.subscribe loc $ \hash translation -> 
          handleAction $ LangChange hash $ BuzgibiBack.getTranslationPage translation

      handleAction (WinResize w) = H.modify_ _ { winWidth = pure w }
      handleAction (LangChange _ xs) = H.modify_ _ { body = Map.lookup "home" xs }
      handleAction Finalize = do
        end <- H.liftEffect getTimestamp
        {start} <- H.get
        sendComponentTime start end loc

content (Just body) = H.render_ body
content Nothing = HH.text "translation not found"