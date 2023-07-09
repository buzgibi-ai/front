module Buzgibi.Component.HTML.Footer ( html ) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)
import Buzgibi.Component.Cookie as Cookie
import  Buzgibi.Component.Copyright as Copyright

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

html = 
  HH.div [css "bottom-container"] 
  [
      HH.slot_ Copyright.proxy unit Copyright.component unit
  ,   HH.slot_ Cookie.proxy unit Cookie.component unit
  ]