module BoundingBox ( BoundingBox(..)
                   , getBoundingBox
                   , hasOverlap
                   , intersection
                   ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helpers.CSS (getComputedStyle, isProperty)
import Helpers.DOM (scrollHeight, scrollWidth)
import Web.HTML (HTMLElement)
import Web.HTML as HTML
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

newtype BoundingBox = BoundingBox
  { left :: Number
  , top :: Number
  , width :: Number
  , height :: Number
  }

instance showBoundingBox :: Show BoundingBox where
  show (BoundingBox b) = show b

getBoundingBox :: HTMLElement -> Aff BoundingBox
getBoundingBox = liftEffect <<< getBoundingBoxImpl

foreign import getBoundingBoxImpl :: HTMLElement -> Effect BoundingBox


-- getBoundingBox :: HTMLElement -> Aff BoundingBox
-- getBoundingBox el = do
--   rect <- getBoundingClientRect el
--   window <- getWindow
--   scrollX <- getScrollX window
--   scrollY <- getScrollY window
--   width <- getWidth rect
--   height <- getHeight rect
--   let left = rect.left + Int.toNumber scrollX
--       top = rect.top + Int.toNumber scrollY
--   pure $ BoundingBox { left, top, width, height }
--   where
--     getWidth rect = do
--       css <- getComputedStyle el
--       sw <- scrollWidth el
--       ifM (isProperty "overflow-x" "visible" css)
--         (pure $ if (sw > rect.width) then sw else rect.width)
--         (pure $ rect.width)

--     getHeight rect = do
--       css <- getComputedStyle el
--       sh <- scrollHeight el
--       ifM (isProperty "overflow-y" "visible" css)
--         (pure $ if (sh > rect.height) then sh else rect.height)
--         (pure $ rect.height)
        
--     getWindow = liftEffect HTML.window
--     getScrollX = liftEffect <<< Window.scrollX
--     getScrollY = liftEffect <<< Window.scrollY
--     getBoundingClientRect = liftEffect <<< HTMLElement.getBoundingClientRect


hasOverlap :: BoundingBox -> BoundingBox -> Boolean
hasOverlap (BoundingBox a) (BoundingBox b) = 
  if a.left > bRight || b.left > aRight
  then false
  else if a.top > bBottom || b.top > aBottom
       then false
       else true
  where
    aRight = a.left + a.width
    bRight = b.left + b.width
    aBottom = a.top + a.height    
    bBottom = b.top + b.height

intersection :: BoundingBox -> HTMLElement -> Aff BoundingBox
intersection (BoundingBox b) el = do
  BoundingBox {left, top, width, height} <- getBoundingBox el
  css <- getComputedStyle el
  {l, w} <- isProperty "overflow-x" "visible" css >>= case _ of
    true ->  pure $ {l: b.left, w: b.width}
    false -> pure $ {l: max b.left left, w: min b.width width}
  {t, h} <- isProperty "overflow-y" "visible" css >>= case _ of
    true ->  pure $ {t: b.top, h: b.height}
    false -> pure $ {t: max b.top top, h: min b.height height}
  pure $ BoundingBox {left: l, top: t, width: w, height: h}
      
  
