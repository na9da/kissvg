module Page ( Page
            , fromHtml
            , toSvg) where

import Prelude

import BoundingBox (BoundingBox, getBoundingBox, hasOverlap, intersection)
import Control.Apply (lift3)
import Data.Array (catMaybes, concat, cons, elem)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Features.Text (Text)
import Features.Text as Text
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle)
import Helpers.CSS as CSS
import Helpers.DOM (scrollHeight, scrollWidth)
import Partial.Unsafe (unsafePartial)
import SVG (SVG, attr, elements, svg)
import Utils (orM)
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
  ifM (isBlackListedNode `orM` isInvisibleNode `orM` isOutsideBbox)
  (pure [])
  (do b <- clipBoundingBox 
      x <- m node
      xs <- concat <$> (traverse (walkDom m b) =<< childNodes)
      pure $ cons x xs)
  where
    isBlackListedNode = pure $ nodeName node `elem` ["SCRIPT", "STYLE"]
    isInvisibleNode = case HTMLElement.fromNode node of
      Nothing -> pure false
      Just el -> isInvisible =<< getComputedStyle el
    isOutsideBbox = case HTMLElement.fromNode node of
      Nothing -> pure false
      Just el -> not <$> (hasOverlap bbox) <$> getBoundingBox el
    clipBoundingBox = case HTMLElement.fromNode node of
      Nothing -> pure bbox
      Just el -> intersection bbox el
    childNodes = liftEffect (Node.childNodes node >>= NodeList.toArray)

featureFromHtml :: Node -> Aff (Maybe Feature)
featureFromHtml node =
  case nodeType of
    TextNode -> (map TextFeature) <$> Text.fromHtml node
    _ -> pure Nothing
  where
    nodeType = unsafePartial (Node.nodeType node)
    

fromHtml :: HTMLElement -> Aff Page
fromHtml el =
  lift3 page (scrollWidth el) (scrollHeight el) features
  where
    features = do
      bbox <- getBoundingBox el
      catMaybes <$> walkDom featureFromHtml bbox node
    node = HMTLElement.toNode el      

featureToSvg :: Feature -> Maybe SVG
featureToSvg = case _ of
  TextFeature text -> Text.toSvg text

toSvg :: Page -> SVG
toSvg (Page p) = svg attrs (elements children)
  where
    children = catMaybes $ featureToSvg <$> p.features
    attrs = [ attr "width" (show p.width <> "px")
            , attr "height" (show p.height <> "px")
            , attr "xmlns" "http://www.w3.org/2000/svg"
            , attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
            , attr "version" "1.1"
            ]
