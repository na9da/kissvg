module Page ( Page
            , fromHtml
            , toSvg) where

import Prelude

import BoundingBox (BoundingBox, getBoundingBox, hasOverlap, intersection)
import Control.Apply (lift3)
import Data.Array (catMaybes, concat, cons, elem, length, range, zip)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Features.Box (Box)
import Features.Box as Box
import Features.Text (Text)
import Features.Text as Text
import FontResolver (FontResolver)
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle, isProperty)
import Helpers.CSS as CSS
import Helpers.DOM (scrollHeight, scrollWidth)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.HTML.Attributes (xmlns)
import Text.Smolder.Markup (Markup, attribute, (!))
import Text.Smolder.SVG (svg)
import Text.Smolder.SVG.Attributes (height, version, width)
import Utils (orM, px)
import Web.DOM (Node)
import Web.DOM.Node (nodeName)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.NodeType (NodeType(..))
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HMTLElement
import Web.HTML.HTMLElement as HTMLElement

data Feature
  = TextFeature Text
  | BoxFeature Box

newtype Page = Page
  { width :: Number
  , height :: Number
  , features :: Array Feature
  }

page :: Number -> Number -> Array Feature -> Page
page width height features = Page {width, height, features}

isInvisible :: CSSStyleDeclaration -> Aff Boolean
isInvisible css = do
  (isProperty "visibility" "hidden") `orM`
  (isProperty "display" "none") `orM`
  (isProperty "clip" "rect(0px, 0px, 0px, 0px)") `orM`
  (isProperty "clip" "rect(1px, 1px, 1px, 1px)")  
  where
    isProperty prop testVal = CSS.isProperty prop testVal css

walkDom :: forall a. (Node -> Aff a) -> BoundingBox -> Node -> Aff(Array a)
walkDom m bbox node =
  ifM (isBlackListedNode `orM` isInvisibleNode)
    (pure [])
    (case HTMLElement.fromNode node of
        Nothing -> visitNode bbox
        Just el -> ifM (isPositionAbsolute el)
                     (visitNode =<< getBoundingBox el)
                     (ifM (isOutsideBbox el)
                       (pure [])
                       (visitNode =<< clipBoundingBox el)))
  where
    visitNode bbox = do
      x <- m node
      xs <- concat <$> (traverse (walkDom m bbox) =<< childNodes)
      pure $ cons x xs
    isBlackListedNode = pure $ nodeName node `elem` ["SCRIPT", "STYLE"]
    isInvisibleNode = case HTMLElement.fromNode node of
      Nothing -> pure false
      Just el -> isInvisible =<< getComputedStyle el
    isPositionAbsolute el =
      isProperty "position" "absolute" =<< getComputedStyle el
    isOutsideBbox el =
      not <$> (hasOverlap bbox) <$> getBoundingBox el
    clipBoundingBox = intersection bbox
    childNodes = liftEffect (Node.childNodes node >>= NodeList.toArray)

featureFromHtml :: FontResolver -> Node -> Aff (Maybe Feature)
featureFromHtml fontResolver node =
  case nodeType of
    TextNode -> (map TextFeature) <$> Text.fromHtml fontResolver node
    ElementNode -> (map BoxFeature) <$> Box.fromHtml node
    _ -> pure Nothing
  where
    nodeType = unsafePartial (Node.nodeType node)
    

fromHtml :: FontResolver -> HTMLElement -> Aff Page
fromHtml fontResolver el =
  lift3 page (scrollWidth el) (scrollHeight el) features
  where
    features = do
      bbox <- getBoundingBox el
      catMaybes <$> walkDom (featureFromHtml fontResolver) bbox node
    node = HMTLElement.toNode el

featureToSvg :: Tuple Int Feature -> Markup Unit
featureToSvg (Tuple id feature) = case feature of
  TextFeature text -> Text.toSvg text 
  BoxFeature box -> Box.toSvg id box
  
toSvg :: Page -> Markup Unit
toSvg (Page p) = svg ! attrs $ for_ (indexed p.features) featureToSvg
  where
    attrs = width (px p.width)
         <> height (px p.height)
         <> xmlns "http://www.w3.org/2000/svg"
         <> attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
         <> version "1.1"
    indexed xs = zip (range 0 (length xs)) xs
