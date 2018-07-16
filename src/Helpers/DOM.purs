module Helpers.DOM ( scrollWidth
                   , scrollHeight
                   ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Web.HTML (HTMLElement)

scrollWidth :: HTMLElement -> Aff Number
scrollWidth = liftEffect <<< scrollWidthImpl

scrollHeight :: HTMLElement -> Aff Number
scrollHeight = liftEffect <<< scrollHeightImpl

foreign import scrollWidthImpl :: HTMLElement -> Effect Number
foreign import scrollHeightImpl :: HTMLElement -> Effect Number
