module Web.Element
  ( scrollTo
  )
  where

import Prelude

import Effect (Effect)

foreign import scrollTo :: String -> Effect Unit
