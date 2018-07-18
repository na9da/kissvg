module FontResolver ( Font
                    , FontResolver
                    , create
                    ) where

import Prelude

import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Tuple (Tuple(..))

type Font =
  { familyName :: String
  , fontName :: String
  }

type FontResolver = String -> String

createRegex :: Array Font -> Either String Regex
createRegex fonts = Regex.regex pattern flags
  where
    pattern = "(?<=[^-0-9a-z]|^)(" <> familyNames <> ")(?=[^-0-9a-z]|$)"
    familyNames = intercalate "|" (map _.familyName fonts)
    flags = Regex.Flags.global <> Regex.Flags.ignoreCase

createFontMap :: Array Font -> Map String String
createFontMap = Map.fromFoldable <<< map (\f -> Tuple f.familyName f.fontName) 

create :: Array Font -> FontResolver
create fonts = resolver (createRegex fonts) (createFontMap fonts)
  where
    resolver :: Either String Regex -> Map String String -> String -> String
    resolver regex fontMap fontDefinition =
      case regex of
        Right r -> Regex.replace' r (replaceFrom fontMap) fontDefinition
        Left _ -> fontDefinition
                  
    replaceFrom fontMap match _ =
      case Map.lookup match fontMap of
        Nothing -> match
        Just font -> font <> ", " <> match


