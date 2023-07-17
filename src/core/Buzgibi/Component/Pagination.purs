module Buzgibi.Component.Pagination
  ( Input
  , calculateCurrentSegment
  , component
  , proxy
  )
  where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Capability.LogMessages (logDebug)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HPExt
import Data.Array
import Data.Maybe (maybe, Maybe (..), isNothing)
import Data.Foldable (for_)
import Data.Int (toNumber, ceil, rem)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.Event.Event (preventDefault)
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (getStore)
import Effect.AVar (tryPut)

proxy = Proxy :: _ "pagination"

loc = "Buzgibi.Component.Pagination"

type Segment = { xs :: Array Int, next :: Maybe Int }

type State = { currenPage :: Int, total :: Int, perpage :: Int, segment :: Maybe Segment }

data Action = Initialize | Next Int MouseEvent | Receive Input

type Input = { total :: Int, perpage :: Int }

component =
  H.mkComponent
  { initialState: \{total, perpage} -> { currenPage: 1, total: total, perpage: perpage, segment: Nothing }
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize
      , receive = Just <<< Receive
      }
  }
  where 
    handleAction Initialize = do
      {currenPage, total, perpage} <- H.get
      H.modify_ _ { segment = calculateCurrentSegment currenPage total perpage }
      H.raise "ewvew"
    handleAction (Next curr ev) = do
      logDebug $ loc <> " ---> switch to page " <> show curr
      H.liftEffect $ preventDefault $ toEvent ev
      { total, perpage } <- H.get
      H.modify_ _ { currenPage = curr, segment = calculateCurrentSegment curr total perpage }
      {paginationVar} <- getStore
      void $ H.liftEffect $ tryPut curr paginationVar
    handleAction (Receive input) = do 
      logDebug $ loc <> " ---> received from parent " <> show input
      {currenPage, perpage} <- H.get
      H.modify_ _ { total = input.total, segment = calculateCurrentSegment currenPage input.total perpage }

render { segment: Nothing } = HH.div_ []
render { currenPage, segment: Just { xs, next } } =
  HH.nav_ 
  [
      HH.ul [css "pagination justify-content-center"] $
      makeCorner (if currenPage == 1 then Nothing else Just (currenPage - 1)) "Previous" `cons`
      (xs <#> \page ->
          HH.li
          [css ("page-item " <> if currenPage == page then "disabled" else mempty)]
          [ HH.a [css ("page-link" <> if currenPage == page then " text-light bg-dark" else mempty), 
            HPExt.href "#", HE.onClick (Next page) ]
            [HH.text (show page)] ]
      ) `snoc` makeCorner next "Next" 
  ]

calculateCurrentSegment :: Int -> Int -> Int -> Maybe Segment
calculateCurrentSegment curr total perPage =
  let pages = ceil (toNumber (total / perPage)) + if rem total perPage > 0 then 1 else 0
      go i ys xxs | length ys == 3 = go i [] (xxs `snoc` ys)
      go i ys xxs | i == pages = xxs `snoc` (ys `snoc` i)
      go i ys xxs = go (i + 1) (ys `snoc` i) xxs 
      segments = go 1 [] []
      next = if curr < pages then Just (curr + 1) else Nothing
      xsm = flip find segments $ maybe false (const true) <<< find ((==) curr)
  in  flip map xsm \xs -> { xs: xs, next: next }

makeCorner (Just page) label = HH.li [css "page-item"] [HH.a [css "page-link", HPExt.href "#", HE.onClick $ Next page ] [HH.text label] ]
makeCorner Nothing label = HH.li [css "page-item disabled"] [HH.a [css "page-link", HPExt.href "#" ] [HH.text label] ]
