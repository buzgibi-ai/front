module Halogen.Html.Raw
  ( HtmlNode(..)
  , Element(..)
  , HtmlAttribute(..)
  , parse
  ) where

import Prelude

data HtmlNode
  = HtmlElement Element
  | HtmlText String
  | HtmlComment String

derive instance eqHtmlNode :: Eq HtmlNode

type Element =
  { name :: String
  , attributes :: Array HtmlAttribute
  , children :: Array HtmlNode
  }

data HtmlAttribute = HtmlAttribute String String

derive instance Eq HtmlAttribute

foreign import parseFromString
  :: (Element -> HtmlNode)
  -> (String -> String -> HtmlAttribute)
  -> (String -> HtmlNode)
  -> (String -> HtmlNode)
  -> String
  -> Array HtmlNode

parse :: String -> Array HtmlNode
parse input = parseFromString HtmlElement HtmlAttribute HtmlText HtmlComment input