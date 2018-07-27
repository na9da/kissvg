module Page ( Page
            , fromHtml
            , toSvg
            ) where

import Prelude

import BoundingBox (BoundingBox)
import Control.Apply (lift3)
import Control.Monad.State (State, evalState, modify)
import Control.Safely (foldM, traverse_)
import Data.Array (cons, elem, snoc, sortBy)
import Data.List (List, catMaybes)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Features.Box (Box, boundingBox, drawRect, hasNewStackingContext, hideOverflow, zIndex)
import Features.Box as Box
import Features.Text (Text)
import Features.Text as Text
import FontResolver (FontResolver)
import Helpers.CSS (CSSStyleDeclaration, getComputedStyle, isProperty)
import Helpers.DOM (scrollHeight, scrollWidth)
import Partial.Unsafe (unsafePartial)
import Text.Smolder.HTML.Attributes (xmlns)
import Text.Smolder.Markup (Attribute, Markup, attribute, empty, (!))
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
  | BoxFeature Box (List Feature)

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
    childNodes node =
      List.fromFoldable <$> liftEffect (NodeList.toArray =<< Node.childNodes node)

fromHtml :: FontResolver -> HTMLElement -> Aff Page
fromHtml fontResolver el =
  lift3 page (scrollWidth el) (scrollHeight el) feature
  where
    feature = featureFromHtml fontResolver (HTMLElement.toNode el)

-- | An overflow clipping area
type Clip = 
  { markup :: Markup Unit
  , attr :: Attribute
  , area :: Maybe BoundingBox
  }

emptyClip :: Clip
emptyClip = { markup: empty
            , attr: mempty
            , area: Nothing
            }

newClip :: Int -> BoundingBox -> Clip
newClip id area =
  let clipId = "overflow-clip-" <> show id
      markup = SVG.clipPath ! attribute "id" clipId $ drawRect area
      attr = clipPath $ "url(#" <> clipId <> ")"
  in { area: Just area, markup, attr }

intersectClip :: Int -> Clip -> Box -> Clip
intersectClip id {area: Just area} box = newClip id (Box.intersect area box)
intersectClip id {area: Nothing} box = newClip id (boundingBox box)

-- | An z-index stacking context
type StackEntry =
  { id :: Int
  , z :: Int
  , markup :: Markup Unit
  }

type Stack = Array StackEntry

stackPush :: Int -> Int -> Markup Unit -> Stack -> Stack
stackPush id z markup stack = snoc stack {id, z, markup}

stackMerge :: Markup Unit -> Stack -> Markup Unit
stackMerge markup stack =
  mergeMarkups $ cons markup $ map _.markup (stackSort stack)
  where
    stackSort :: Stack -> Stack
    stackSort =
      sortBy (\a b -> if a.z == b.z
                      then compare a.id b.id
                      else compare a.z b.z) 


mergeMarkups :: Array (Markup Unit) -> Markup Unit
mergeMarkups [] = empty
mergeMarkups xs = g $ traverse_ identity xs

-- | Build markup for a feature tree starting at `feature`
-- |
-- | Markups are built according to the nodes stacking order
-- | given by its z-index. We also apply clipping if parent has overflow
-- | set to not visible.
buildMarkup :: Clip -> Stack -> Feature -> State Int Stack
buildMarkup parentClip parentStack feature = do
  id <- incrementId
  case feature of
    TextFeature text ->
      let markup = Text.toSvg id text ! parentClip.attr
      in pure $ stackPush id 0 markup parentStack
    BoxFeature box children -> do
      let boxMarkup = Box.toSvg id box ! parentClip.attr
          Tuple markup clip =
            if hideOverflow box
            then attachClip boxMarkup (intersectClip id parentClip box)
            else Tuple boxMarkup parentClip
          z = zIndex box
      if hasNewStackingContext box
        then do
          childStack <- walkChildren clip [] children
          pure $ stackPush id z (stackMerge markup childStack) parentStack
        else do
          let parentStack' = stackPush id z markup parentStack
          walkChildren clip parentStack' children
  where
    walkChildren :: Clip -> Stack -> List Feature -> State Int Stack
    walkChildren clip stack = foldM (buildMarkup clip) stack

    incrementId :: State Int Int
    incrementId = modify (\i -> i + 1)

    attachClip :: Markup Unit -> Clip -> Tuple (Markup Unit) Clip
    attachClip bodyMarkup clip =
      let markup = mergeMarkups [bodyMarkup, clip.markup]
      in Tuple markup clip

featureToSvg :: Feature -> Markup Unit
featureToSvg feature =
  let stack = evalState (buildMarkup emptyClip mempty feature) 0
  in stackMerge empty stack

toSvg :: Page -> Markup Unit
toSvg (Page p) = svg ! attrs $ maybe empty featureToSvg p.feature
  where
    attrs = xmlns "http://www.w3.org/2000/svg"
         <> version "1.1"  
         <> attribute "width" (px p.width)
         <> attribute "height" (px p.height)
         <> attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"


