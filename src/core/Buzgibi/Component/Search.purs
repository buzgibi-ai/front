module Buzgibi.Component.Search (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HPExt

proxy = Proxy :: _ "search"

loc = "Buzgibi.Component.Search"

component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

-- https://codepen.io/AustinAuth/pen/xxmbZX
render = 
  HH.div [css "search-container"] 
  [
      HH.form [css "search-container-form"]
      [
          HH.input  
          [ HPExt.placeholder "What can I help you with today?"
          , HPExt.type_ HPExt.InputText
          , HPExt.id "search-bar"
          ]
      ,   HH.a [ HPExt.href "#" ] [HH.img [ css "search-icon", HPExt.src "http://www.endlessicons.com/wp-content/uploads/2012/12/search-icon.png" ]]    
      ]
  ]