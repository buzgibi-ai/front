module Buzgibi.Api.Foreign.User.Api
  ( Enquiry
  , History
  , HistoryItem
  , Location
  , UserApi
  , getHistory
  , makeEnquiry
  , mkUserApi
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
import Data.Maybe (Maybe, fromMaybe)

foreign import data UserApi :: Type

foreign import mkUserApi :: Fn1 ApiClient (Effect UserApi)

type Location = { latitude :: Number, longitude :: Number  }

type Enquiry = { enquiry :: String, location :: Location }

foreign import _makeEnquiry :: Fn3 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Enquiry UserApi (AC.EffectFnAff (Object (Response Unit)))

makeEnquiry :: Enquiry -> UserApi -> (AC.EffectFnAff (Object (Response Unit)))
makeEnquiry = runFn3 _makeEnquiry withError

type HistoryItem = { ident :: Int, name :: String, timestamp :: String }

type History = { items :: Array HistoryItem, total :: Int, perpage :: Int }

type Page = { page :: Int }

foreign import _getHistory :: Fn3 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Page UserApi (AC.EffectFnAff (Object (Response History)))

getHistory :: Maybe Page -> UserApi -> (AC.EffectFnAff (Object (Response History)))
getHistory page = runFn3 _getHistory withError (fromMaybe {page: 1} page)

