module Features.Text ( Text
                     , fromHtml
                     , toSvg
                     ) where

import Prelude

import BoundingBox (BoundingBox(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Effect.Aff (Aff)
import Helpers.CSS (buildCss, css, getComputedStyle, getPropertyValue, singleQuote)
import Helpers.DOM (parentElement)
import Helpers.Range (Line(..))
import Helpers.Range as Range
import Partial.Unsafe (unsafePartial)
import SVG (SVG, attr, content, elements)
import SVG as SVG
import Utils (parseFloat, regex)
import Web.DOM (Node)
import Web.HTML (HTMLElement)

type TextStyle =
  { fontSize :: String
  , fontFamily :: String
  , fontWeight :: String
  , fontStyle :: String
  , color :: String
  }

newtype Text = Text
  { lines :: Array Line
  , style :: TextStyle
  }

text :: Array Line -> TextStyle -> Text
text lines style = Text {lines, style}

getTextStyle :: HTMLElement -> Aff TextStyle
getTextStyle el = do
  css <- getComputedStyle el
  textStyle
    <$> getPropertyValue "font-size" css
    <*> getPropertyValue "font-family" css
    <*> getPropertyValue "font-weight" css
    <*> getPropertyValue "font-style" css
    <*> getPropertyValue "color" css
  where
    textStyle = 
      { fontSize: _
      , fontFamily: _
      , fontWeight: _
      , fontStyle: _
      , color: _ 
      }

fromHtml :: Node -> Aff (Maybe Text)
fromHtml node = Range.getWrappedLines node >>= case _ of
  [] -> pure Nothing
  lines -> Just <$> text lines <$> (getTextStyle =<< parentElement node)
  

toSvg :: Text -> Maybe SVG
toSvg (Text {lines, style}) = 
  Just $ case lines of
    [Line str bbox] -> SVG.text (positionAttrs bbox <> styleAttrs) (content str)
    _ -> SVG.text styleAttrs (elements (tspan <$> lines))
  where
    tspan (Line str bbox) = SVG.tspan (positionAttrs bbox) (lstrip " " str)
    positionAttrs (BoundingBox b) = unsafePartial $
      [ attr "x" (show b.left)
      , attr "y" (show (b.top + fromJust (parseFloat style.fontSize)))
      ]
    styleAttrs =
      [ attr "style" $ buildCss [ css "font-size" style.fontSize
                                , css "font-weight" style.fontWeight
                                , css "font-style" style.fontStyle
                                ]
      , attr "font-family" (singleQuote style.fontFamily)
      , attr "fill" style.color
      , attr "xml:space" "preserve"
      ]
    lstrip s = Regex.replace (regex "^ " Regex.Flags.noFlags) ""
