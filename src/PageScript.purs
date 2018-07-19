module PageScript where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FontResolver (Font)
import FontResolver as FontResolver
import Helpers.DOM as DOM
import Page as Page
import Text.Smolder.Renderer.String (render)

foreign import setGlobalVariable :: forall a. String -> a -> Effect Unit

renderSvg :: Array Font -> Effect (Promise String)
renderSvg fonts = Promise.fromAff $ do
  let fontResolver = FontResolver.create fonts
  DOM.querySelector "html" >>= case _ of
    Nothing -> pure ""
    Just el -> render <$> Page.toSvg <$> Page.fromHtml fontResolver el

main :: Effect Unit
main = setGlobalVariable "$kissvg" {renderSvg}
