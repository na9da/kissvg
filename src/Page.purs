module Page ( Page
            , fromHtml
            , toSvg
            ) where

import Prelude

import BoundingBox (BoundingBox(..))
import Control.Apply (lift3)
import Control.Monad.ST as ST
import Control.Monad.State (evalState, get, modify)
import Data.Array (catMaybes, cons, elem, foldM, length, range, snoc, sortBy, zip)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (trace)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Features.Box (Box, bbox, className, clipChildren, drawRect, newStack, zIndex)
import Features.Box as Box
import Features.Text (Text)
import Features.Text as Text
import FontResolver (FontResolver)
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle, isProperty)
import Helpers.DOM (scrollHeight, scrollWidth)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.HTML.Attributes (xmlns)
import Text.Smolder.Markup (Markup, attribute, empty, text, (!))
import Text.Smolder.SVG (g, svg)
import Text.Smolder.SVG as SVG
import Text.Smolder.SVG.Attributes (clipPath, version)
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

-- require cleanup -- 

featureToSvg :: Feature -> Markup Unit
featureToSvg feature' =
  case feature' of
    TextFeature _ -> empty
    BoxFeature box _ ->
      let clip = {ref: mempty, area: bbox box, markup: Nothing}
          stack = evalState (iter clip [] feature') 0
      in mergeStack empty stack
  where
    iter parentClip parentStack feature = do
      id <- incrementId
      case feature of
        TextFeature text -> do
          let markup = Text.toSvg id text ! parentClip.ref
          pushStack 0 markup parentStack
        BoxFeature box children -> do
          let boxMarkup = Box.toSvg id box ! parentClip.ref
              newClip = intersect id parentClip box
              markup = case newClip.markup of
                Nothing -> boxMarkup
                Just m -> merge [m, boxMarkup]
              z = zIndex box
          if newStack box
            then do
              stack <- foldM (iter newClip) ([]) children
              pushStack z (mergeStack markup stack) parentStack
            else do
              stack <- pushStack z markup parentStack
              foldM (iter newClip) stack children

    incrementId = modify (\id -> id + 1)

    pushStack z markup stack = do
      id <- get
      pure $ snoc stack {id, z, markup}

    mergeStack parentMarkup stack =
      merge (cons parentMarkup (sortStack stack))

    merge [] = empty
    merge xs = g $ traverse_ identity xs

    sortStack stack =
      let stack' = sortBy (\a b -> if a.z == b.z
                                   then compare a.id b.id
                                   else compare a.z b.z) stack
      in _.markup <$> stack'

    intersect id clip@{area} box =
      if clipChildren box
      then let newArea = Box.intersect area box
           in { area: newArea
              , ref: clipPath ("url(#overflow-clip" <> show id <> ")")
              , markup: Just $drawClip id newArea !attribute "className" (className box)
              }
      else clip {markup = Nothing}

    drawClip clipId area =
      SVG.clipPath ! attribute "id" ("overflow-clip" <> show clipId) $ drawRect area


toSvg :: Page -> Markup Unit
toSvg (Page p) = svg ! attrs $ maybe empty featureToSvg p.feature
  where
    attrs = xmlns "http://www.w3.org/2000/svg"
         <> version "1.1"  
         <> attribute "width" (px p.width)
         <> attribute "height" (px p.height)
         <> attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"


