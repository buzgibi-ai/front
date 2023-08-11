module Buzgibi.Component.Subscription.WS (subscribe) where

import Prelude

import Halogen as H
import Web.Socket as WS
import Web.Socket.ReadyState
import Control.Monad.Rec.Class (forever)
import Effect.Aff as Aff

import Undefined

subscribe loc ws goCompHandle = do
  ws <- H.liftEffect $ WS.create ws []
  let isOpen = do
        Aff.delay $ Aff.Milliseconds 1000.0
        st <-  H.liftEffect $ WS.readState ws
        if st == Open then pure true else isOpen
  H.liftAff $ unlessM isOpen $ pure unit
  void $ H.fork $ forever $ do 
    H.liftAff $ Aff.delay $ Aff.Milliseconds 1000.0
    st <- H.liftEffect $ WS.readState ws
    when (st == Open) undefined