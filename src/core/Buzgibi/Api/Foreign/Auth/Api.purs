module Buzgibi.Api.Foreign.Auth.Api
  ( AuthApi
  , ResponseAuthToken
  , mkAuthApi
  , register
  )
  where

import Prelude

import Buzgibi.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn3, runFn3)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Either (Either)
import Effect.Exception as E

foreign import data AuthApi :: Type
foreign import data ResponseAuthToken :: Type

foreign import mkAuthApi :: Fn1 ApiClient (Effect AuthApi)

type Credentials = { email :: String, password :: String }

foreign import _register :: Fn3 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Credentials AuthApi (AC.EffectFnAff (Object ResponseAuthToken))

register :: Credentials -> AuthApi -> (AC.EffectFnAff (Object ResponseAuthToken))
register cred = runFn3 _register withError cred
