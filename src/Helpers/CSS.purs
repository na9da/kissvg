module Helpers.CSS ( CSSStyleDeclaration
                   , CSS
                   , css
                   , buildCss
                   , singleQuote
                   , getComputedStyle
                   , getPropertyValue
                   , isProperty
                   ) where

import Prelude

import Data.Array (intercalate)
import Data.String (replaceAll)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Web.HTML (HTMLElement)

data CSS = CSS String String

instance showCSS :: Show CSS where
  show (CSS prop val) = prop <> ":" <> singleQuote val

css :: String -> String -> CSS
css = CSS

buildCss :: Array CSS -> String
buildCss = intercalate "; " <<< map show

singleQuote :: String -> String
singleQuote = replaceAll (String.Pattern "\"") (String.Replacement "'")

foreign import data CSSStyleDeclaration :: Type

getComputedStyle :: HTMLElement -> Aff CSSStyleDeclaration
getComputedStyle = liftEffect <<< getComputedStyleImpl

getPropertyValue :: String -> CSSStyleDeclaration -> Aff String
getPropertyValue name = liftEffect <<< getPropertyValueImpl name

isProperty :: String -> String -> CSSStyleDeclaration -> Aff Boolean
isProperty name testVal s = (eq testVal) <$> getPropertyValue name s

foreign import getComputedStyleImpl :: HTMLElement -> Effect CSSStyleDeclaration
foreign import getPropertyValueImpl :: String -> CSSStyleDeclaration -> Effect String

