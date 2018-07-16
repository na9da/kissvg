module SVG where

import Prelude

import Data.Array (intercalate)
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags)
import Data.String.Regex.Flags as Regex.Flags
import Partial.Unsafe (unsafePartial)

data Tag
  = Svg
  | Defs
  | Pattern
  | G
  | Text
  | Tspan
  | Image
  | Path
  | Rect

instance showTag :: Show Tag where
  show Svg = "svg"
  show Defs = "defs"
  show Pattern = "pattern"
  show G = "g"
  show Text = "text"
  show Tspan = "tspan"  
  show Image = "image"
  show Path = "path"
  show Rect = "rect"


data Inner
  = Content String
  | Elements (Array SVG)
  | Empty

escapeText :: String -> String
escapeText t =
  let specialChars = regex "[\"&'<>]" Regex.Flags.noFlags
  in if isJust (Regex.match specialChars  t)
     then "<![CDATA[" <> t <> "]]>"
     else t

instance showInner :: Show Inner where
  show (Elements xs) = intercalate "" (show <$> xs)
  show (Content s) = escapeText s
  show Empty = ""

data Attr = Attr String String

regex :: String -> RegexFlags -> Regex
regex r flags = unsafePartial $ fromRight $ Regex.regex r flags    

escapeAttrVal :: String -> String
escapeAttrVal =
  Regex.replace' (regex "[\"&]" Regex.Flags.global)
    (\match _ -> case match of
        "&" -> "&amp;"
        "\"" -> "'"
        x -> x)

instance showAttr :: Show Attr where
  show (Attr name val) =
    name <> "=" <> "\"" <> escapeAttrVal val <> "\""

newtype SVG = SVG
  { tag :: Tag
  , attrs :: Array Attr
  , inner :: Inner
  }

instance showSVG :: Show SVG where
  show (SVG {tag, attrs, inner}) =
    tagOpen <> show inner <> tagClose
    where
      tagOpen = "<" <> show tag <> attrStr <> ">"
      tagClose = "</" <> show tag <> ">"
      attrStr =
        case intercalate " " (show <$> attrs) of
          "" -> ""
          s -> " " <> s

attr :: String -> String -> Attr
attr = Attr

elements :: Array SVG -> Inner
elements = Elements

content :: String -> Inner
content = Content

svg :: Array Attr -> Inner -> SVG
svg attrs inner =
  SVG { tag: Svg
      , attrs
      , inner
      }
