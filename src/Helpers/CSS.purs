module Helpers.CSS ( CSSStyleDeclaration
                   , CSS
                   , Position(..)
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
                   , parsePosition
                   ) where

import Prelude

import Data.Array (intercalate)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Maybe (Maybe(..))
import Data.String (replaceAll)
import Data.String as String
import Data.String.Regex as Regex
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
  case String.split (String.Pattern " ") s of
    [x] -> x == "repeat" || x == "repeat-x"
    [x, _] -> x == "repeat" || x == "repeat-x"
    _ -> false

isRepeatY :: String -> Boolean
isRepeatY s =
  case String.split (String.Pattern " ") s of
    [y] -> y == "repeat" || y == "repeat-y"
    [_, y] -> y == "repeat" || y == "repeat-y"
    _ -> false

data Position
  = Percentage Number
  | Pixel Number

parsePosition :: String -> Maybe Position
parsePosition s = do
  matches <- Regex.match r s
  case Array.NonEmpty.toArray matches of
    [Just parsed, _, Just num, _, Just u] -> do
      unit <- decodeUnit u
      x <- parseFloat num
      let rest = String.drop (String.length parsed) s
      Just (unit x)
    _ -> Nothing
  where
    r = regex "^((\\d+(\\.\\d+)?)(%|px))" Regex.Flags.noFlags
    decodeUnit = case _ of
      "%" -> Just Percentage
      "px" -> Just Pixel
      _ -> Nothing
    
foreign import getComputedStyleImpl :: HTMLElement -> Effect CSSStyleDeclaration
foreign import getPropertyValueImpl :: String -> CSSStyleDeclaration -> Effect String

