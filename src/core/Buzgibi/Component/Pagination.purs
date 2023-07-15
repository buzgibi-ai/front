module Buzgibi.Component.Pagination (component, proxy) where

import Prelude

import Buzgibi.Component.HTML.Utils (css)

import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Halogen.HTML.Properties.Extended as HPExt

proxy = Proxy :: _ "pagination"

loc = "Buzgibi.Component.Pagination"

type State = { currenPage :: Int, isNext :: Boolean }


component =
  H.mkComponent
  { initialState: identity
    , render: render
    , eval: H.mkEval H.defaultEval
  }

render { currenPage, isNext }  =
  HH.nav_ 
  [
      HH.ul [css "pagination justify-content-center"]
      [
          HH.li [css ("page-item " <> if currenPage == 1 then "disabled" else mempty)]
          [
              HH.a [css "page-link text-dark", HPExt.href "#"] [HH.text "Previous"]
          ]
      ,   HH.li [css "page-item"] [ HH.a [css "page-link text-dark", HPExt.href "#"] [HH.text "1"] ]
      ,   HH.li [css "page-item"] [ HH.a [css "page-link text-light bg-dark", HPExt.href "#"] [HH.text "2"] ]
      ,   HH.li [css ("page-item " <> if isNext == false then "disabled" else mempty)]
          [
              HH.a [css "page-link text-dark", HPExt.href "#"] [HH.text "Next"]
          ]
      ]
  ] 
