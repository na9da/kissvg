module BoundingBox ( BoundingBox(..)
                   , getBoundingBox
                   ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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

