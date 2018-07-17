module BoundingBox ( BoundingBox(..)
                   , getBoundingBox
                   , hasOverlap
                   , intersection
                   ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Helpers.CSS (getComputedStyle, isProperty)
import Web.HTML (HTMLElement)

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
      
  
