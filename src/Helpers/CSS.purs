module Helpers.CSS ( CSSStyleDeclaration
                   , CSS
                   , Units(..)
                   , css
                   , buildCss
                   , singleQuote
                   , getComputedStyle
                   , getPropertyValue
                   , isProperty
                   , parseBackgroundUrl
                   , transparentColor
                   , isRepeatX
                   , isRepeatY
                   , parseUnits
                   ) where

import Prelude

import Data.Array (intercalate)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Maybe (Maybe(..))
import Data.String (replaceAll)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Flags as Regex.Flags
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Utils (parseFloat, regex)
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

transparentColor :: String
transparentColor = "rgba(0, 0, 0, 0)"

foreign import data CSSStyleDeclaration :: Type

getComputedStyle :: HTMLElement -> Aff CSSStyleDeclaration
getComputedStyle = liftEffect <<< getComputedStyleImpl

getPropertyValue :: String -> CSSStyleDeclaration -> Aff String
getPropertyValue name = liftEffect <<< getPropertyValueImpl name

isProperty :: String -> String -> CSSStyleDeclaration -> Aff Boolean
isProperty name testVal s = (eq testVal) <$> getPropertyValue name s

parseBackgroundUrl :: String -> Maybe String
parseBackgroundUrl val = 
  case val of
    "none" -> Nothing
    _ -> parse
  where
    parse = do
      matches <- Regex.match (regex "url\\(\"(.*?)\"\\)" Regex.Flags.noFlags) val
      url <- Array.NonEmpty.last matches
      pure url

isRepeatX :: String -> Boolean
isRepeatX s =
  case Regex.split (regex "[^-0-9a-z]+" ignoreCase) s of
    [x] -> x == "repeat" || x == "repeat-x"
    [x, _] -> x == "repeat" || x == "repeat-x"
    _ -> false

isRepeatY :: String -> Boolean
isRepeatY s =
  case Regex.split (regex "[^-0-9a-z]+" ignoreCase) s of
    [y] -> y == "repeat" || y == "repeat-y"
    [_, y] -> y == "repeat" || y == "repeat-y"
    _ -> false

data Units
  = Percentage Number
  | Pixel Number

parseUnits :: String -> Maybe Units
parseUnits s = do
  matches <- Regex.match r s
  case Array.NonEmpty.toArray matches of
    [_, Just num, _, Just u] -> do
      unit <- decodeUnits u
      x <- parseFloat num
      Just (unit x)
    _ -> Nothing
  where
    r = regex """(-?\d+(\.\d+)?)(%|px)""" Regex.Flags.noFlags
    decodeUnits = case _ of
      "%" -> Just Percentage
      "px" -> Just Pixel
      _ -> Nothing

foreign import getComputedStyleImpl :: HTMLElement -> Effect CSSStyleDeclaration
foreign import getPropertyValueImpl :: String -> CSSStyleDeclaration -> Effect String

