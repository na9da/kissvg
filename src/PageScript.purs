module PageScript where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Page as Page
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

foreign import setGlobalVariable :: forall a. String -> a -> Effect Unit

renderSvg :: Effect (Promise String)
renderSvg = Promise.fromAff $ do
  body <- liftEffect $ HTML.window >>= Window.document >>= HTMLDocument.body
  case body of
    Nothing -> pure ""
    Just el -> show <$> Page.toSvg <$> Page.fromHtml el

main :: Effect Unit
main = setGlobalVariable "$kissvg" {renderSvg}
