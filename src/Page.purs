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
import FontResolver (FontResolver)
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle, isProperty)
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
