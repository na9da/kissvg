module Features.Box where

import Prelude

import BoundingBox (BoundingBox(..), getBoundingBox)
import Control.Apply (lift2, lift3, lift4)
import Data.Array (all)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeEncodeURI)
import Helpers.CSS (CSSStyleDeclaration, Units(..), getComputedStyle, getPropertyValue, isRepeatX, isRepeatY, parseBackgroundUrl, parseUnits, transparentColor)
import Helpers.DOM (getImageDimension)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.Markup (Markup, attribute, empty, (!))
import Text.Smolder.SVG (g, image, pattern, rect)
import Text.Smolder.SVG as SVG
import Text.Smolder.SVG.Attributes (clipPath, fill, height, patternUnits, rx, ry, stroke, strokeWidth, width, x, y)
import Utils (lift6, parseInt, parseNumber, px)
import Web.DOM (Node)
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLImageElement as HTMLImageElement

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
            , size :: String
            }
 , zIndex :: Int
 , overflow :: { x :: String, y :: String }
 }

newtype Box = Box
  { bbox :: BoundingBox
  , style :: BoxStyle
  , className :: String
  }

boundingBox :: Box -> BoundingBox
boundingBox (Box {bbox}) = bbox

zIndex :: Box -> Int
zIndex (Box {style}) = style.zIndex

newStack :: Box -> Boolean
newStack box = zIndex box > 0

hasNewStackingContext :: Box -> Boolean
hasNewStackingContext box = zIndex box > 0

hideOverflow :: Box -> Boolean
hideOverflow (Box {style}) =
  style.overflow.x /= "visible" || style.overflow.y /= "visible"

intersect :: BoundingBox -> Box -> BoundingBox
intersect (BoundingBox a) (Box {bbox, style}) =
  let BoundingBox {left, top, width, height} = bbox
      right = left + width
      bottom = top + height
      aRight = a.left + a.width
      aBottom = a.top + a.height
      {l, w} = if style.overflow.x == "visible" 
               then {l: a.left, w: a.width}
               else let l = max a.left left
                        w = if l > aRight
                            then 0.0
                            else min aRight right - l
                    in {l, w}
      {t, h} = if style.overflow.y == "visible"
               then {t: a.top, h: a.height}
               else let t = max a.top top
                        h =  if t > aBottom
                             then 0.0
                             else min aBottom bottom - t
                    in {t, h}
  in BoundingBox {left: l, top: t, width: w, height: h}


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
      Just <$> (lift3 { url, width, height, position: _, repeat: _, size: _ }
                getPosition
                (getPropertyValue "background-repeat" css)
                (getPropertyValue "background-size" css))
  where
    backgroundUrl = parseBackgroundUrl <$> getPropertyValue "background-image" css
    getPosition = do
      x <- getPropertyValue "background-position-x" css
      y <- getPropertyValue "background-position-y" css
      pure $ {x, y}

getImage :: HTMLElement -> Aff (Maybe _)
getImage el = case HTMLImageElement.fromNode (HTMLElement.toNode el) of
  Nothing -> pure Nothing
  Just img -> do
    BoundingBox bbox <- getBoundingBox el
    let size = px bbox.width <> " " <> px bbox.height
        position = { x: "0px", y: "0px" }
    width <- naturalWidth img
    height <- naturalHeight img
    if (width == 0.0 || height == 0.0)
      then pure Nothing
      else Just <$> ({position, width, height, repeat: "", size, url: _} <$> src img)
  where
    src = liftEffect <<< HTMLImageElement.src
    naturalWidth img = Int.toNumber <$> (liftEffect $ HTMLImageElement.naturalWidth img)
    naturalHeight img = Int.toNumber<$>(liftEffect $ HTMLImageElement.naturalHeight img)

getZIndex :: CSSStyleDeclaration -> Aff Int
getZIndex css = 
  (fromMaybe 0 <<< parseInt) <$> getPropertyValue "z-index" css

getOverflow :: CSSStyleDeclaration -> Aff _
getOverflow css =
  lift2 { x: _, y: _ }
    (getPropertyValue "overflow-x" css)
    (getPropertyValue "overflow-y" css)  

getBoxStyle :: HTMLElement -> Aff BoxStyle
getBoxStyle el = do
  css <- getComputedStyle el
  lift6 mkStyle
     (getZIndex css)
     (getBorders css)
     (getCorners css)
     (getPropertyValue "background-color" css)
     (getBackgroundImage css >>= case _ of
         Just bg -> pure (Just bg)
         Nothing -> getImage el)
     (getOverflow css)
  where
    mkStyle = { zIndex: _
              , borders: _
              , corners: _
              , backgroundColor: _
              , backgroundImage: _
              , overflow: _
              }

fromHtml :: Node -> Aff (Maybe Box)
fromHtml node = 
  case HTMLElement.fromNode node of
    Nothing -> pure Nothing
    Just el -> Just <$> lift3 box (getBoundingBox el) (getBoxStyle el) (className el)
  where
    className :: HTMLElement -> Aff String
    className = liftEffect <<< HTMLElement.className

    box :: BoundingBox -> BoxStyle -> String -> Box
    box bbox style className = Box {bbox, style, className}

      
drawRect :: BoundingBox -> Markup Unit
drawRect (BoundingBox b) = rect ! attrs
  where attrs = x (px b.left)
         <> y (px b.top)
         <> width (px b.width)
         <> height (px b.height)

drawFillRect ::  BoundingBox -> String -> Markup Unit
drawFillRect bbox fillColor = drawRect bbox ! fill fillColor

drawClipRect :: String -> Box -> Markup Unit
drawClipRect clipId (Box {bbox, style}) =
  SVG.clipPath ! attribute "id" clipId $
    drawRect bbox ! rx (px corner) ! ry (px corner)
  where
    corner = style.corners.tl

drawBorderRect :: BoundingBox -> Border -> Markup Unit
drawBorderRect (BoundingBox b) border =
  drawRect borderBox ! strokeWidth (px border.width) ! stroke border.color ! fill "none"
  where
    borderBox =
      BoundingBox { left: b.left + border.width / 2.0
                  , top: b.top  + border.width / 2.0
                  , width: b.width - border.width
                  , height: b.height - border.width
                  }
      
drawBackgroundBox :: String -> BoundingBox -> _ -> Markup Unit
drawBackgroundBox patId bbox@(BoundingBox b) style = do
  pattern ! patternAttrs $ (image ! imageAttrs $ empty)
  drawRect bbox ! fill patternUrl
  where
    patternAttrs = attribute "id" patId
                   <> patternUnits "userSpaceOnUse"
                   <> width (px patternWidth)
                   <> height (px patternHeight)
                   <> x (px $ b.left + borders.left.width + xoff)
                   <> y (px $ b.top + borders.top.width + yoff)
    patternWidth =
      if isRepeatX backgroundImage.repeat
      then bgWidth
      else let w = max b.width bgWidth
           in max w (w - xoff) --adjust for negative shift due to background positioning

    patternHeight =
      if isRepeatY backgroundImage.repeat
      then bgHeight
      else let h = max b.height bgHeight
           in max h (h - yoff) --adjust for negative shift due to background positioning

    xoff = case parseUnits backgroundImage.position.x of
      Just (Percentage p) -> pcnt p b.width - pcnt p bgWidth
      Just (Pixel p) -> p
      Nothing -> 0.0

    yoff = case parseUnits backgroundImage.position.y of
      Just (Percentage p) -> pcnt p b.height - pcnt p bgHeight
      Just (Pixel p) -> p
      Nothing -> 0.0

    pcnt p num = p * num / 100.0

    imageAttrs = attribute "xlink:href" (unsafeEncodeURI backgroundImage.url)
                 <> x (px 0.0)
                 <> y (px 0.0)
                 <> width (px bgWidth)
                 <> height (px bgHeight)

    bgWidth = case parseUnits backgroundImage.size of
      Nothing -> backgroundImage.width
      Just (Pixel p) -> p
      Just (Percentage p) -> pcnt p b.width

    bgHeight = case parseUnits backgroundImage.size of
      Nothing -> backgroundImage.height
      Just _ -> backgroundImage.height * bgWidth / backgroundImage.width

    backgroundImage = unsafePartial $ fromJust style.backgroundImage
    borders = style.borders
    patternUrl = "url(" <> "#" <> patId <> ")"


drawBox :: Int -> Box -> Markup Unit
drawBox id box@(Box {bbox, style}) = g $ do
  if requireClipping
    then drawClipRect clipId box
    else empty
  if style.backgroundColor /= transparentColor
    then drawFillRect bbox style.backgroundColor ! clipReference
    else empty
  if hasBackgroundImage
    then drawBackgroundBox patternId bbox style ! clipReference
    else empty
  if hasBorders
    then drawBorderRect bbox style.borders.top ! clipReference
    else empty
  where
    requireClipping = style.corners.tl /= 0.0
    hasBorders =
      let {width, color} = style.borders.top
      in width /= 0.0 && color /= transparentColor
    hasBackgroundImage = isJust style.backgroundImage
    clipReference = if requireClipping
                      then clipPath ("url(" <> "#" <> clipId <> ")")
                      else mempty
    clipId = "border-clip" <> show id
    patternId = "pat" <> show id

toSvg :: Int -> Box -> Markup Unit
toSvg id box@(Box {bbox, style, className}) = do
  if isInvisible
    then empty
    else drawBox id box ! attribute "className" className
  where
    isInvisible =
      (width == 0.0 || height == 0.0) ||
      (allBordersSame
      && (borders.top.width == 0.0 || borders.top.color == transparentColor)
      && style.backgroundColor == transparentColor
      && isNothing style.backgroundImage)

    width =
      let BoundingBox {width} = bbox
      in width

    height =
      let BoundingBox {height} = bbox
      in height

    allBordersSame =
      let {top, right, bottom, left} = borders
      in all (eq top) [right, bottom, left]

    borders = style.borders

