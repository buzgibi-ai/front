module TTHouse.Api.Foreign.Request.Handler (withError, onFailure) where

import Prelude

import TTHouse.Data.Route (Route (Error500))
import TTHouse.Capability.LogMessages (class LogMessages)
import TTHouse.Capability.Now (class Now)
import TTHouse.Capability.Navigate (class Navigate)
import TTHouse.Capability.Navigate (navigate)

import Data.Either (Either (..))
import TTHouse.Capability.LogMessages (logError)
import Store (Action (WriteError))
import Halogen.Store.Monad (updateStore)
import Effect.Exception (Error)
import Halogen.Query.HalogenM (HalogenM)
import Effect.Aff.Class (class MonadAff)
import Halogen.Store.Monad (class MonadStore)
import Store (Action, Store)

withError 
  :: forall m a s xs ys o . 
  LogMessages m => 
  Now m => 
  MonadAff m => 
  MonadStore Action Store m => 
  Navigate m => 
  Either Error a -> 
  (a -> HalogenM s xs ys o m Unit) -> 
  HalogenM s xs ys o m Unit
withError (Right x) success = success x
withError (Left e) _ = logError (show e) *> updateStore (WriteError e) *> navigate Error500

onFailure  
  :: forall m a s xs ys o . 
  LogMessages m => 
  Now m => 
  MonadAff m => 
  MonadStore Action Store m => 
  Navigate m => 
  Either Error a -> 
  (Error -> HalogenM s xs ys o m Unit) ->
  (a -> HalogenM s xs ys o m Unit) -> 
  HalogenM s xs ys o m Unit
onFailure (Right x) _ success = success x
onFailure (Left e) failure _ = failure e