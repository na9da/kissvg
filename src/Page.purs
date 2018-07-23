module Page ( Page
            , fromHtml
            , toSvg
            ) where

import Prelude

import Control.Apply (lift3)
import Control.Monad.State (evalState, modify)
import Data.Array (catMaybes, elem)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Features.Box (Box)
import Features.Box as Box
import Features.Text (Text)
import Features.Text as Text
import FontResolver (FontResolver)
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle, isProperty)
import Helpers.DOM (scrollHeight, scrollWidth)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.HTML.Attributes (xmlns)
import Text.Smolder.Markup (Markup, attribute, empty, (!))
import Text.Smolder.SVG (g, svg)
import Text.Smolder.SVG.Attributes (version)
import Utils (orM, px)
import Web.DOM (Node)
import Web.DOM.Node (nodeName)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.NodeType (NodeType(..))
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

data Feature
  = TextFeature Text
  | BoxFeature Box (Array Feature)

newtype Page = Page
  { width :: Number
  , height :: Number
  , feature :: Maybe Feature
  }

page :: Number -> Number -> Maybe Feature -> Page 
page width height feature = Page { width, height, feature }

isHidden :: CSSStyleDeclaration -> Aff Boolean
isHidden css =
   isProperty "visibility" "hidden" css `orM`
   isProperty "display" "none" css

featureFromHtml :: FontResolver -> Node -> Aff (Maybe Feature)
featureFromHtml fontResolver node =
  ifM (isBlacklistedNode `orM` isHiddenNode)
    (pure Nothing)
    (case nodeType of
        TextNode -> map TextFeature <$> Text.fromHtml fontResolver node
        ElementNode -> Box.fromHtml node >>= case _ of
          Nothing -> pure Nothing
          Just box -> Just <$> BoxFeature box <$> children
        _ -> pure Nothing)
  where
    isBlacklistedNode = pure $ nodeName node `elem` ["HEAD", "SCRIPT", "STYLE"]
    isHiddenNode = case HTMLElement.fromNode node of
      Nothing -> pure false
      Just el -> isHidden =<< getComputedStyle el
    nodeType = unsafePartial $ Node.nodeType node
    children =
      catMaybes <$> (traverse (featureFromHtml fontResolver) =<< childNodes node)
    childNodes = liftEffect <<< (NodeList.toArray <=< Node.childNodes)

fromHtml :: FontResolver -> HTMLElement -> Aff Page
fromHtml fontResolver el =
  lift3 page (scrollWidth el) (scrollHeight el) feature
  where
    feature = featureFromHtml fontResolver (HTMLElement.toNode el)

featureToSvg :: Feature -> Markup Unit
featureToSvg feature' = evalState (iter feature') 0
  where
    iter feature = do
      id <- modify ((+) 1)
      case feature of
        TextFeature text -> pure $ Text.toSvg id text
        BoxFeature box children -> do
          descendants <- merge <$> for children iter
          pure $ Box.toSvg id box descendants
    merge [] = empty
    merge xs = g $ traverse_ identity xs

toSvg :: Page -> Markup Unit
toSvg (Page p) = svg ! attrs $ maybe empty featureToSvg p.feature
  where
    attrs = xmlns "http://www.w3.org/2000/svg"
         <> version "1.1"  
         <> attribute "width" (px p.width)
         <> attribute "height" (px p.height)
         <> attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"


