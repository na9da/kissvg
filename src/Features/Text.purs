module Features.Text ( Text
                     , fromHtml
                     , toSvg
                     ) where

import Prelude

import BoundingBox (BoundingBox(..))
import Data.Array (cons, init, last, snoc, uncons)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import FontResolver (FontResolver)
import Helpers.CSS (buildCss, css, getComputedStyle, getPropertyValue, isProperty, singleQuote)
import Helpers.DOM (innerText, parentElement)
import Helpers.DOM as DOM
import Helpers.Range (Line(..))
import Helpers.Range as Range
import Partial.Unsafe (unsafePartial)
import SVG (SVG, attr, content, elements)
import SVG as SVG
import Utils (parseFloat, regex)
import Web.DOM (Node)
import Web.DOM.Node (nextSibling, previousSibling)
import Web.DOM.Text (wholeText)
import Web.DOM.Text as DOM.Text
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

type TextStyle =
  { fontSize :: String
  , fontFamily :: String
  , fontWeight :: String
  , fontStyle :: String
  , color :: String
  }

newtype Text = Text
  { lines :: Array Line
  , style :: TextStyle
  }

text :: Array Line -> TextStyle -> Text
text lines style = Text {lines, style}

getTextStyle :: FontResolver -> HTMLElement -> Aff TextStyle
getTextStyle resolveFont el = do
  css <- getComputedStyle el
  textStyle
    <$> getPropertyValue "font-size" css
    <*> (resolveFont <$> getPropertyValue "font-family" css)
    <*> getPropertyValue "font-weight" css
    <*> getPropertyValue "font-style" css
    <*> getPropertyValue "color" css
  where
    textStyle = 
      { fontSize: _
      , fontFamily: _
      , fontWeight: _
      , fontStyle: _
      , color: _ 
      }

isLeftOf :: Node -> Node -> Aff Boolean
isLeftOf a b = do
  xa <- DOM.x a
  xb <- DOM.x b
  pure $ xa < xb
  
hasText :: Node -> Aff Boolean
hasText node = (notEq "") <$> getText node

getText :: Node -> Aff String
getText node = do
  case DOM.Text.fromNode node of
    Just textNode -> liftEffect $ wholeText textNode
    Nothing -> case HTMLElement.fromNode node of
      Just el -> innerText el
      Nothing -> pure ""

endsWith :: String -> Node -> Aff Boolean
endsWith pat node =
  Regex.test (regex (pat <> "$") Regex.Flags.noFlags) <$> getText node

-- | Trim all white space at the beginning of the line
trimLeft :: Line -> Aff Line
trimLeft (Line s bbox) =
  let s' = Regex.replace (regex "^\\s+" Regex.Flags.noFlags) "" s
  in pure $ Line s' bbox

-- | Trim all but one white space at the beginning of the line
trimLeft1 :: Line -> Aff Line
trimLeft1 (Line s bbox) =
  let s' = Regex.replace (regex "^\\s+(?=\\s)" Regex.Flags.noFlags) "" s
  in pure $ Line s' bbox

-- | Trim all white space at the end of the line
trimRight :: Line -> Aff Line
trimRight (Line s bbox) =
  let s' = Regex.replace (regex "\\s+$" Regex.Flags.noFlags) "" s
  in pure $ Line s' bbox

-- | Trim all but one white space at the end of the line
trimRight1 :: Line -> Aff Line
trimRight1 (Line s bbox) =
  let s' = Regex.replace (regex "(?<=\\s)\\s+$" Regex.Flags.noFlags) "" s
  in pure $ Line s' bbox

-- | Return the previous node that is non empty and is in the same line as this one
prevNonEmptyInlineNode :: Node -> Aff (Maybe Node)
prevNonEmptyInlineNode node =
  liftEffect (previousSibling node) >>= case _ of
    Just sib ->
      ifM (sib `isLeftOf` node)
        (ifM (hasText sib)
           (pure $ Just sib)         
           (prevNonEmptyInlineNode sib))
        (pure Nothing)
    Nothing -> do
      parent <- parentElement node
      parentCss <- getComputedStyle parent
      ifM (isProperty "display" "inline" parentCss)
        (prevNonEmptyInlineNode (HTMLElement.toNode parent))
        (pure Nothing)

-- | Return the next node that is non empty and is in the same line as this one
nextNonEmptyInlineNode :: Node -> Aff (Maybe Node)
nextNonEmptyInlineNode node = do
  liftEffect (nextSibling node) >>= case _ of
    Just sib ->
      ifM (node `isLeftOf` sib)
        (ifM (hasText sib)
           (pure $ Just sib)         
           (nextNonEmptyInlineNode sib))
        (pure Nothing)
    Nothing -> do
      parent <- parentElement node
      parentCss <- getComputedStyle parent
      ifM (isProperty "display" "inline" parentCss)
        (nextNonEmptyInlineNode (HTMLElement.toNode parent))
        (pure Nothing)

-- | Normalize whitespace by mimicking the browsers behavior
normalizeSpace :: Node -> Array Line -> Aff (Array Line)
normalizeSpace node =
  trimFirst >=> trimTail >=> trimLast
  where
    trimFirst lines = case uncons lines of
      Nothing -> pure lines
      Just {head, tail} -> do
        head' <- prevNonEmptyInlineNode node >>= case _ of
          Nothing -> trimLeft head
          Just p -> ifM (endsWith " " p) (trimLeft head) (trimLeft1 head)
        pure $ cons head' tail
    trimTail lines = case uncons lines of
      Nothing -> pure lines
      Just {head, tail} -> do
        tail' <- traverse trimLeft tail
        pure $ cons head tail'
    trimLast lines = case last lines of
      Nothing -> pure lines
      Just l -> do
        last' <- nextNonEmptyInlineNode node >>= case _ of
          Nothing -> trimRight l
          Just n -> trimRight1 l
        pure $ snoc (fromMaybe [] (init lines)) last'
        

fromHtml :: FontResolver -> Node -> Aff (Maybe Text)
fromHtml fontResolver node = do
  lines <- normalizeSpace node =<< Range.getWrappedLines node
  case lines of
    [] -> pure Nothing
    ls -> do
      style <- getTextStyle fontResolver =<< parentElement node
      pure $ Just (text lines style)

toSvg :: Text -> Maybe SVG
toSvg (Text {lines, style}) = 
  Just $ case lines of
    [Line str bbox] -> SVG.text (positionAttrs bbox <> styleAttrs) (content str)
    lines' -> SVG.text styleAttrs (elements (tspan <$> lines'))
  where
    tspan (Line str bbox) = SVG.tspan (positionAttrs bbox) str
    positionAttrs (BoundingBox b) = unsafePartial $
      [ attr "x" (show b.left)
      , attr "y" (show (b.top + fromJust (parseFloat style.fontSize)))
      ]
    styleAttrs =
      [ attr "style" $ buildCss [ css "font-size" style.fontSize
                                , css "font-weight" style.fontWeight
                                , css "font-style" style.fontStyle
                                ]
      , attr "font-family" (singleQuote style.fontFamily)
      , attr "fill" style.color
      , attr "xml:space" "preserve"
      ]
