module Features.Text ( Text
                     , fromHtml
                     , toSvg
                     ) where

import Prelude

import BoundingBox (BoundingBox(..))
import Data.Array (cons, init, last, snoc, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Traversable (for_, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FontResolver (FontResolver)
import Helpers.CSS (getComputedStyle, getPropertyValue, isProperty, singleQuote)
import Helpers.DOM (innerText, parentElement)
import Helpers.DOM as DOM
import Helpers.Range (Line(..))
import Helpers.Range as Range
import Text.Smolder.Markup (Markup, attribute, text, (!))
import Text.Smolder.SVG as SVG
import Text.Smolder.SVG.Attributes (fill, fontFamily, fontSize, fontStyle, fontWeight, x, y)
import Utils (parseNumber, px, regex)
import Web.DOM (Node)
import Web.DOM.Node (nextSibling, previousSibling)
import Web.DOM.Text (wholeText)
import Web.DOM.Text as DOM.Text
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

type TextStyle =
  { fontSize :: Number
  , fontFamily :: String
  , fontWeight :: String
  , fontStyle :: String
  , color :: String
  }

newtype Text = Text
  { lines :: Array Line
  , style :: TextStyle
  }

getTextStyle :: FontResolver -> HTMLElement -> Aff TextStyle
getTextStyle resolveFont el = do
  css <- getComputedStyle el
  textStyle
    <$> (parseNumber 0.0 <$> getPropertyValue "font-size" css)
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
    ls -> Just <$> text ls <$> (getTextStyle fontResolver =<< parentElement node)
  where
    text :: Array Line -> TextStyle -> Text
    text lines style = Text {lines, style}

toSvg :: Int -> Text -> Markup Unit
toSvg id (Text {lines, style}) =
  case lines of
    [Line l b] -> SVG.text ! (positionAttrs b <> styleAttrs) $ text l
    lines' -> SVG.text ! styleAttrs $ for_ lines' tspan
  where
    tspan (Line l b) = SVG.tspan ! (positionAttrs b) $ text l

    positionAttrs (BoundingBox b) = 
      x (px b.left) <>
      y (px $ b.top + style.fontSize)

    styleAttrs =  fontFamily (singleQuote style.fontFamily)
               <> fill style.color
               <> attribute "xml:space" "preserve"
               <> fontSize (px style.fontSize)
               <> fontWeight style.fontWeight
               <> fontStyle style.fontStyle

