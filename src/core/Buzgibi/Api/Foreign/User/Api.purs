module Buzgibi.Api.Foreign.User.Api
  ( History
  , Location
  , Survey
  , UserApi
  , WithFieldStatusHistoryItem
  , getHistory
  , makeSurvey
  , mkUserApi
  , printWithFieldStatusHistoryItem
  )
  where

import Prelude

import Buzgibi.Api.Foreign.Common

import Effect (Effect)
import Data.Function.Uncurried (Fn1, Fn3, runFn3)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign, typeOf)
import Data.Either (Either)
import Effect.Exception as E
import Data.Maybe (Maybe, fromMaybe)

foreign import data UserApi :: Type

foreign import mkUserApi :: Fn1 ApiClient (Effect UserApi)

type Location = { latitude :: Number, longitude :: Number }

type Survey = 
     { survey :: String
     , assessmentscore :: String
     , category :: String
     , phonesfileident :: Int
     , location :: Location
     }

foreign import _makeSurvey :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Survey UserApi (AC.EffectFnAff (Object (Response Unit)))

makeSurvey :: Survey -> UserApi -> (AC.EffectFnAff (Object (Response Unit)))
makeSurvey = runFn3 _makeSurvey withError

type WithFieldStatusHistoryItem = { ident :: Foreign, status :: String, name :: String, timestamp :: String }

printWithFieldStatusHistoryItem { ident, status, name, timestamp } = "{ ident:" <> typeOf ident <> ", status:" <> status <> ", name: " <> name <> ", tm: "  <> timestamp <> " }" 

type History = { items :: Array WithFieldStatusHistoryItem, total :: Int, perpage :: Int }

type Page = { page :: Int }

foreign import _getHistory :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Page UserApi (AC.EffectFnAff (Object (Response History)))

getHistory :: Maybe Page -> UserApi -> (AC.EffectFnAff (Object (Response History)))
getHistory page = runFn3 _getHistory withError (fromMaybe { page: 1 } page)

