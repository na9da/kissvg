module Features.Box where

import Prelude

import BoundingBox (BoundingBox, getBoundingBox)
import Control.Apply (lift2, lift4)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle, getPropertyValue, parseBackgroundUrl)
import Helpers.DOM (getImageDimension)
import SVG (SVG)
import Utils (parseNumber)
import Web.DOM (Node)
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

type Border = { width :: Number, color :: String }
type BoxStyle =
 { borders :: { top :: Border, right :: Border, bottom :: Border, left :: Border }
 , corners :: { tl :: Number, tr :: Number, br :: Number, bl :: Number }
 , backgroundColor :: String
 , backgroundImage ::
      Maybe { url :: String
            , width :: Number
            , height :: Number
            , position :: { x :: String, y :: String }
            , repeat :: String
            }
 }

newtype Box = Box
  { bbox :: BoundingBox
  , style :: BoxStyle
  }

box :: BoundingBox -> BoxStyle -> Box
box bbox style = Box {bbox, style}

getBorders :: CSSStyleDeclaration -> Aff _
getBorders css =
  lift4 {top: _, right: _, bottom: _, left: _}
      (border "top")
      (border "right")
      (border "bottom")
      (border "left")
  where
    border side = do
      color <- getPropertyValue ("border-" <> side <> "-color") css
      width <- parseNumber 0.0 <$> getPropertyValue ("border-" <> side <> "-width") css
      pure $ { width, color }

getCorners :: CSSStyleDeclaration -> Aff _
getCorners css =
  lift4 {tl: _, tr: _, br: _, bl: _}
      (corner "top-left")
      (corner "top-right")
      (corner "bottom-right")
      (corner "bottom-left")
  where
    corner c = do
      parseNumber 0.0 <$> getPropertyValue ("border-" <> c <> "-radius") css

getBackgroundImage :: CSSStyleDeclaration -> Aff (Maybe _)
getBackgroundImage css = backgroundUrl >>= case _ of
  Nothing -> pure Nothing
  Just url -> getImageDimension url >>= case _ of
    Nothing -> pure Nothing
    Just {width, height} ->
      Just <$> (lift2 { url, width, height, position: _, repeat: _ }
                getPosition
                (getPropertyValue "background-repeat" css))
  where
    backgroundUrl = parseBackgroundUrl <$> getPropertyValue "background-image" css
    getPosition = do
      x <- getPropertyValue "background-position-x" css
      y <- getPropertyValue "background-position-y" css
      pure $ {x, y}

getBoxStyle :: HTMLElement -> Aff BoxStyle
getBoxStyle el = do
  css <- getComputedStyle el
  lift4 {borders: _, corners: _, backgroundColor: _, backgroundImage: _}
     (getBorders css)
     (getCorners css)
     (getPropertyValue "background-color" css)
     (getBackgroundImage css)

fromHtml :: Node -> Aff (Maybe Box)
fromHtml node = 
  case HTMLElement.fromNode node of
    Nothing -> pure Nothing
    Just el -> Just <$> lift2 box (getBoundingBox el) (getBoxStyle el)

toSvg :: Int -> Box -> Maybe SVG
toSvg id box = Nothing
