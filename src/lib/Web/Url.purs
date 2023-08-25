module Web.Url (getQueryParam) where

import Prelude

import Data.Maybe (Maybe(..))

foreign import _getQueryParam :: Maybe String -> (String -> Maybe String) -> String -> Maybe String

getQueryParam :: String -> Maybe String
getQueryParam = _getQueryParam Nothing Just