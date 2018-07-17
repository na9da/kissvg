module PageScript where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Helpers.DOM as DOM
import Page as Page

foreign import setGlobalVariable :: forall a. String -> a -> Effect Unit

renderSvg :: Effect (Promise String)
renderSvg = Promise.fromAff $ do
  DOM.querySelector "html" >>= case _ of
    Nothing -> pure ""
    Just el -> show <$> Page.toSvg <$> Page.fromHtml el

main :: Effect Unit
main = setGlobalVariable "$kissvg" {renderSvg}
