-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module TTHouse.Component.Root
  ( Action(..)
  , ChildSlots
  , Query(..)
  , State
  , component
  )
  where

import Prelude

import TTHouse.Component.Utils (OpaqueSlot)
import TTHouse.Data.Route (Route(..), routeCodec)
import TTHouse.Page.Home as Home
import TTHouse.Page.Service as Service
import TTHouse.Page.About as About
import TTHouse.Page.Error.Page500 as Page500
import TTHouse.Page.Error.Page404 as Page404 
import TTHouse.Capability.Navigate
import TTHouse.Capability.LogMessages (class LogMessages, logDebug)
import TTHouse.Capability.Now (class Now)
import TTHouse.Component.HTML.Header as Header
import TTHouse.Component.HTML.Footer as Footer
import TTHouse.Component.HTML.Body as Body
import TTHouse.Component.Lang.Data (Lang (..))
import TTHouse.Component.Async as Async
import TTHouse.Component.Root.Fork.Translation as Fork.Translation 
import TTHouse.Component.Root.Fork.Telegram as Fork.Telegram
import TTHouse.Component.HTML.Loading as HTML.Loading
import TTHouse.Data.Config

import Data.Either (hush, Either (..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Undefined
import Halogen.HTML.Properties as HP
import Store (Store, printStore)
import Store as Store
import Store.Types (Platform)
import Effect.AVar as Async
import Data.Foldable (for_)
import Data.List (head)
import Data.Map as Map
import Routing.Duplex.Parser (RouteError (EndOfPath))

loc = " TTHouse.Component.Root"

data Query a = Navigate Route a

type State = { route :: Maybe Route, lang :: Lang }

data Action = Initialize | LangChange Lang

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , error500 :: OpaqueSlot Unit
  , about :: OpaqueSlot Unit
  , service :: OpaqueSlot Unit
  , error404 :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => Navigate m
  => LogMessages m
  => Now m
  => MonadStore Store.Action Store m
  => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const { route: Nothing, lang: Eng }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction Initialize = do
    logDebug $ loc <> " ---> root component init start .."
    store@{ config: Config {isCaptcha} } <- getStore
    logDebug $ printStore store

    -- show up info if captcha is disabled
    when (not isCaptcha) $
      Async.send $ Async.mkOrdinary "captcha is disabled" Async.Info Nothing


    Fork.Telegram.init >>= Fork.Telegram.fork

    Fork.Translation.init
    Fork.Translation.fork $ handleAction <<< LangChange

    -- first we'll get the route the user landed on
    from <-(RD.parse routeCodec) <$> liftEffect getHash
    -- then we'll navigate to the new route (also setting the hash)
    logDebug $ loc <> " ---> root component init is about to be completed .."
    case from of 
      Right route -> navigate route 
      Left EndOfPath -> navigate Home
      Left _ -> navigate Error404
  handleAction (LangChange lang) = H.modify_ _ { lang = lang }
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery (Navigate dest a) = do
    store <- getStore
    logDebug $ printStore store
    H.modify_ _ { route = pure dest }
    pure $ Just a

params = 
  { header: Header.html
  , footer: Footer.html
  }

render :: forall m
  . MonadAff m
  => Navigate m
  => LogMessages m
  => Now m
  => MonadStore Store.Action Store m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { route: Nothing } = HTML.Loading.html  
render { route: Just r@Home } = HH.slot_ Home.proxy unit (Home.component (Body.mkBodyHtml params r)) unit
render { route: Just Error500 } = HH.slot_ Page500.proxy unit Page500.component unit
render { route: Just r@About } = HH.slot_ About.proxy unit (About.component (Body.mkBodyHtml params r)) unit
render { route: Just r@Service } = HH.slot_ Service.proxy unit (Service.component (Body.mkBodyHtml params r)) unit
render { route: Just Error404 } = HH.slot_ Page404.proxy unit Page404.component unit