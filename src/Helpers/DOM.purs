module Helpers.DOM ( scrollWidth
                   , scrollHeight
                   , parentElement
                   , innerText
                   , y
                   , x
                   , querySelector
                   , getImageDimension
                   ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
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

x :: Node -> Aff Number
x = liftEffect <<< xImpl

querySelector :: String -> Aff (Maybe HTMLElement)
querySelector = liftEffect <<< querySelectorImpl Nothing Just

getImageDimension :: String -> Aff (Maybe {width :: Number, height :: Number})
getImageDimension url = 
  catchError
    (Just <$> (fromEffectFnAff $ getImageDimensionImpl url))
    (const (pure Nothing))

foreign import scrollWidthImpl :: HTMLElement -> Effect Number
foreign import scrollHeightImpl :: HTMLElement -> Effect Number
foreign import parentElementImpl :: Node -> Effect HTMLElement
foreign import innerTextImpl :: HTMLElement -> Effect String
foreign import yImpl :: Node -> Effect Number
foreign import xImpl :: Node -> Effect Number
foreign import querySelectorImpl
  :: forall a
   . Maybe a
  -> (a -> Maybe a)
  -> String
  -> Effect (Maybe HTMLElement)
foreign import getImageDimensionImpl :: String -> EffectFnAff {width :: Number, height :: Number}
