module Buzgibi.Api.Foreign.User.Api
  ( History
  , Location
  , SubmitSurvey
  , Survey
  , UserApi
  , WithFieldStatusHistoryItem
  , getHistory
  , makeSurvey
  , mkUserApi
  , printWithFieldStatusHistoryItem
  , submitSurvey
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

makeSurvey :: Survey -> UserApi -> AC.EffectFnAff (Object (Response Unit))
makeSurvey = runFn3 _makeSurvey withError

type SubmitSurvey = { ident :: Int } 

foreign import _submitSurvey :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) SubmitSurvey UserApi (AC.EffectFnAff (Object (Response Unit)))

submitSurvey :: SubmitSurvey -> UserApi -> AC.EffectFnAff (Object (Response Unit))
submitSurvey = runFn3 _submitSurvey withError

type WithFieldStatusHistoryItem = 
     { surveyident :: Int,
       reportident :: Foreign, 
       status :: String, 
       name :: String, 
       timestamp :: String,
       voice :: Foreign
     }

printWithFieldStatusHistoryItem { surveyident, reportident, status, name, timestamp, voice } = 
  "{ surveyident: " <> show surveyident <>
  "reportident:" <> typeOf reportident <> 
  ", status:" <> status <> 
  ", name: " <> name <> 
  ", tm: "  <> timestamp <> 
  ", voice: <bytes> }" 

type History = { items :: Array WithFieldStatusHistoryItem, total :: Int, perpage :: Int }

type Page = { page :: Int }

foreign import _getHistory :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Page UserApi (AC.EffectFnAff (Object (Response History)))

getHistory :: Maybe Page -> UserApi -> (AC.EffectFnAff (Object (Response History)))
getHistory page = runFn3 _getHistory withError (fromMaybe { page: 1 } page)