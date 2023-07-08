module Buzgibi.Api.Foreign.Common
  ( ApiClient
  , Error
  , JWTToken(..)
  , Response
  , getDataFromObj
  , mkApiClient
  , withError
  , JWTStatus (..)
  )
  where

import Prelude

import Effect.Exception as E
import Effect
import Foreign.Object (Object)
import Data.Either
import Data.Function.Uncurried (Fn1)
import Foreign (Foreign)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError (TypeMismatch))
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Generic.Rep (class Generic)

foreign import data ApiClient :: Type
foreign import data Response :: Type -> Type
foreign import data Error :: Type

instance showError :: Show Error where
  show = _printError

foreign import _printError :: Error -> String

foreign import _getDataFromObj 
  :: forall a b . 
  (String -> Either E.Error b) -> 
  (b -> Either E.Error b) -> 
  Object a -> 
  Effect (Either E.Error b)

getDataFromObj :: forall a b . Object a -> Effect (Either E.Error b)
getDataFromObj = _getDataFromObj (Left <<< E.error) Right

foreign import mkApiClient :: Fn1 String (Effect ApiClient)

foreign import withError :: forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a 

newtype JWTToken = JWTToken String

instance EncodeJson JWTToken where
  encodeJson (JWTToken token) = "token" := token ~> jsonEmptyObject

data JWTStatus = Valid | Invalid | Skip

derive instance Generic JWTStatus _

instance DecodeJson JWTStatus where
  decodeJson = genericDecodeJson