module Buzgibi.Component.Subscription.Pagination (subscribe) where

import Prelude

import Halogen as H
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff
import Halogen.Store.Monad (getStore)
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref

subscribe loc goCompHandle =
  void $ H.fork $ forever $ do
    H.liftAff $ Aff.delay $ Aff.Milliseconds 500.0
    { paginationVar } <- getStore
    H.liftEffect (Ref.read paginationVar) >>= goCompHandle
    