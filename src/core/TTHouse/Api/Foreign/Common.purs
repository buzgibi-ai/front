module TTHouse.Api.Foreign.Common
  ( ApiClient
  , Error
  , Response
  , getDataFromObj
  , mkApiClient
  , withError
  )
  where

import Prelude

import Effect.Exception as E
import Effect
import Foreign.Object (Object)
import Data.Either
import Data.Function.Uncurried (Fn1)
import Foreign (Foreign)

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