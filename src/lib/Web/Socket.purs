module Web.Socket
  ( Protocol(..)
  , WebSocket
  , create
  , readState
  )
  where

import Prelude

import Data.Function.Uncurried (Fn2, Fn1, runFn2, runFn1)
import Data.Unit
import Effect
import Partial.Unsafe (unsafePartial)
import Web.Socket.ReadyState (toEnumReadyState, ReadyState)
import Data.Maybe (fromJust)
import Effect.Aff.Compat as AC
import Data.Either

newtype Protocol = Protocol String

foreign import data WebSocket :: Type

foreign import _create :: Fn2 String (Array Protocol) (Effect WebSocket)

foreign import _readState :: Fn1 WebSocket (Effect Int)

create :: String -> Array Protocol -> Effect WebSocket
create = runFn2 _create

readState :: WebSocket -> Effect ReadyState
readState ws = do 
  st <- runFn1 _readState ws
  pure $ unsafePartial $ fromJust $ toEnumReadyState st