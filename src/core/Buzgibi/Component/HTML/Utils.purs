module Buzgibi.Component.HTML.Utils
  ( css
  , maybeElem
  , safeHref
  , whenElem
  , whenElemf
  ) where

import Prelude

import Buzgibi.Data.Route (Route, routeCodec)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: forall r i. Route -> HH.IProp (href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- | Sometimes we need to deal with elements which may or may not exist. This function lets us
-- | provide rendering for the element if it exists, and renders an empty node otherwise.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | PureScript is a strict language. If we want to conditionally display an element, then we
-- | should hide the evaluation behind a function, which won't be evaluated right away, in order
-- | to minimize the work performed each render.
whenElemf :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElemf cond f = if cond then f unit else HH.text ""

whenElem :: forall p i. Boolean -> HH.HTML p i -> HH.HTML p i
whenElem cond el = if cond then el else HH.div_ []

