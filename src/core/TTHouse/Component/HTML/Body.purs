module TTHouse.Component.HTML.Body
  ( Body
  , BodyHtml(..)
  , Content
  , Footer
  , Header
  , mkBodyHtml
  )
  where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import TTHouse.Component.HTML.Utils (css)
import Halogen.HTML.Properties.Extended as HPExt

type Content i p = HH.HTML i p
type Header i p = HH.HTML i p 
type Body i p = HH.HTML i p
type Footer i p = HH.HTML i p

type BodyHtml = 
     { header :: forall i p . Header i p
     , footer :: forall i p . Footer i p
     }

mkBodyHtml { header, footer } route platform width content = 
  HH.div_ [ header route platform width, footer, contentWrapper content ]

contentWrapper content =
  HH.table [HPExt.style "margin: 0 auto; width:100%;"]
  [
      HH.thead_ 
      [
         HH.tr [HPExt.style "width:100%;"]
         [
             HH.td_ [HH.div [css "page-header-space"] []]
         ]
      ]
  ,   HH.tbody_
      [
         HH.tr [HPExt.style "width:100%;height:900px"]
         [
            HH.td_ [HH.div [css "content-wrapper"] [content]] 
         ]
      ]
  ]