module Page ( Page
            , fromHtml
            , toSvg) where

import Prelude

import Control.Apply (lift3)
import Effect.Aff (Aff)
import Helpers.DOM (scrollHeight, scrollWidth)
import SVG (SVG, attr, elements, svg)
import Web.HTML (HTMLElement)

data Feature
  = TextFeature

newtype Page = Page
  { width :: Number
  , height :: Number
  , features :: Array Feature
  }

page :: Number -> Number -> Array Feature -> Page
page width height features = Page {width, height, features}

fromHtml :: HTMLElement -> Aff Page
fromHtml el =
  lift3 page (scrollWidth el) (scrollHeight el) features
  where
    features = pure []

toSvg :: Page -> SVG
toSvg (Page p) = svg attrs (elements children)
  where
    children = []
    attrs = [ attr "width" (show p.width <> "px")
            , attr "height" (show p.height <> "px")
            , attr "xmlns" "http://www.w3.org/2000/svg"
            , attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
            , attr "version" "1.1"
            ]
