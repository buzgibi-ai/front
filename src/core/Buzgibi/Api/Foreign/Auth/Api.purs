module Buzgibi.Api.Foreign.Auth.Api
  ( AuthApi
  , AuthType(..)
  , ResponseAuthToken
  , confirmEmail
  , login
  , logout
  , mkAuthApi
  , register
  , sendResetPasswordLink
  , setNewPassword
  ) where

import Prelude

import Buzgibi.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn2, Fn4, runFn4, runFn3)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Either (Either)
import Effect.Exception as E
import Data.Maybe (Maybe)

foreign import data AuthApi :: Type
foreign import data ResponseAuthToken :: Type

foreign import mkAuthApi :: Fn1 ApiClient (Effect AuthApi)

type Credentials = { email :: String, password :: String }

foreign import _register :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Credentials AuthApi (AC.EffectFnAff (Object ResponseAuthToken))

register :: Credentials -> AuthApi -> (AC.EffectFnAff (Object ResponseAuthToken))
register cred = runFn3 _register withError cred

data AuthType = Jwt

stringifyAuthType :: AuthType -> String
stringifyAuthType Jwt = "jwt"

foreign import _login :: Fn4 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String Credentials AuthApi (AC.EffectFnAff (Object ResponseAuthToken))

login :: Credentials -> AuthApi -> (AC.EffectFnAff (Object ResponseAuthToken))
login = runFn4 _login withError (stringifyAuthType Jwt)

foreign import _logout :: Fn2 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) AuthApi (AC.EffectFnAff (Object (Response Unit)))

logout :: AuthApi -> AC.EffectFnAff (Object (Response Unit))
logout = runFn2 _logout withError

type Emailconfirm = { key :: String }

foreign import _confirmEmail :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Emailconfirm AuthApi (AC.EffectFnAff (Object (Response Boolean)))

confirmEmail :: Emailconfirm -> AuthApi -> AC.EffectFnAff (Object (Response Boolean))
confirmEmail = runFn3 _confirmEmail withError

foreign import _sendResetPasswordLink :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String AuthApi (AC.EffectFnAff (Object (Response (Maybe Int))))

sendResetPasswordLink :: String -> AuthApi -> AC.EffectFnAff (Object (Response (Maybe Int)))
sendResetPasswordLink = runFn3 _sendResetPasswordLink withError

type NewPassword = { password :: String, key :: String }

foreign import _setNewPassword :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) NewPassword AuthApi (AC.EffectFnAff (Object (Response Boolean)))

setNewPassword :: NewPassword -> AuthApi -> AC.EffectFnAff (Object (Response Boolean))
setNewPassword = runFn3 _setNewPassword withError
