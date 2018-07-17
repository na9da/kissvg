module Helpers.DOM ( scrollWidth
                   , scrollHeight
                   , parentElement
                   , innerText
                   , y
                   ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Web.DOM (Node)
import Web.HTML (HTMLElement)

scrollWidth :: HTMLElement -> Aff Number
scrollWidth = liftEffect <<< scrollWidthImpl

scrollHeight :: HTMLElement -> Aff Number
scrollHeight = liftEffect <<< scrollHeightImpl

parentElement :: Node -> Aff HTMLElement
parentElement = liftEffect <<< parentElementImpl

innerText :: HTMLElement -> Aff String
innerText = liftEffect <<< innerTextImpl

y :: Node -> Aff Number
y = liftEffect <<< yImpl

foreign import scrollWidthImpl :: HTMLElement -> Effect Number
foreign import scrollHeightImpl :: HTMLElement -> Effect Number
foreign import parentElementImpl :: Node -> Effect HTMLElement
foreign import innerTextImpl :: HTMLElement -> Effect String
foreign import yImpl :: Node -> Effect Number
