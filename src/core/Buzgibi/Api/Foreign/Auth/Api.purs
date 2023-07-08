module Buzgibi.Api.Foreign.Auth.Api
  ( AuthApi
  , AuthType(..)
  , ResponseAuthToken
  , login
  , mkAuthApi
  , register
  )
  where

import Prelude

import Buzgibi.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn3, runFn3, Fn4, runFn4)
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

data AuthType = Jwt 

stringifyAuthType :: AuthType -> String
stringifyAuthType Jwt = "jwt"

foreign import _login :: Fn4 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String Credentials AuthApi (AC.EffectFnAff (Object ResponseAuthToken))

login :: Credentials -> AuthApi -> (AC.EffectFnAff (Object ResponseAuthToken))
login = runFn4 _login withError (stringifyAuthType Jwt)
