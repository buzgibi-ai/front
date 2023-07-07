module Buzgibi.Component.HTML.Footer ( html ) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Cookie as Cookie

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

html = 
  HH.div [css "page-footer", HP.style "text-align: center"] 
  [ HH.slot_ Cookie.proxy unit Cookie.component unit ]