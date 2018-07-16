module Helpers.Range ( Line(..)
                     , getWrappedLines
                     ) where

import Prelude

import BoundingBox (BoundingBox)
import Control.Apply (lift2)
import Data.Array (singleton, zipWith)
import Data.Array as Array
import Data.String.CodeUnits as String
import Data.String.Unsafe as String.Unsafe
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Utils (whileM)
import Web.DOM (Node)

data Line = Line String BoundingBox

foreign import data Range :: Type

getWrappedLines :: Node -> Aff (Array Line)
getWrappedLines = create >=> getWrappedLines'

getWrappedLines' :: Range -> Aff (Array Line)
getWrappedLines' range = do
  getLineBoundingBoxes range >>= case _ of
    [] -> mempty
    [b] -> singleton <$> (Line <$> toString range <*> pure b)
    bs -> zipWith Line <$> whileM notEmpty readLine <*> pure bs
  where
    readLine = do
      n <- lineCount
      chars <- whileM (lift2 (&&) notEmpty (lineCountIs n)) readChar
      pure $ String.fromCharArray chars

    readChar = do
      node <- startContainer range
      pos <- startOffset range
      char <- String.Unsafe.charAt 0 <$> toString range
      setStart node (pos + 1) range
      pure char

    lineCount = Array.length <$> getLineBoundingBoxes range
    lineCountIs n = eq n <$> lineCount
    notEmpty = not <$> collapsed range


create :: Node -> Aff Range
create = liftEffect <<< createImpl

setStart :: Node -> Int -> Range -> Aff Unit
setStart node pos = liftEffect <<< setStartImpl node pos

startContainer :: Range -> Aff Node
startContainer = liftEffect <<< startContainerImpl

startOffset :: Range -> Aff Int
startOffset = liftEffect <<< startOffsetImpl

collapsed :: Range -> Aff Boolean
collapsed = liftEffect <<< collapsedImpl

toString :: Range -> Aff String
toString = liftEffect <<< toStringImpl

getLineBoundingBoxes :: Range -> Aff (Array BoundingBox)
getLineBoundingBoxes = liftEffect <<< getLineBoundingBoxesImpl

foreign import createImpl :: Node -> Effect Range
foreign import setStartImpl :: Node -> Int -> Range -> Effect Unit
foreign import startContainerImpl :: Range -> Effect Node
foreign import startOffsetImpl :: Range -> Effect Int
foreign import collapsedImpl :: Range -> Effect Boolean
foreign import toStringImpl :: Range -> Effect String
foreign import getLineBoundingBoxesImpl :: Range -> Effect (Array BoundingBox)



