module PageScript where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)

foreign import setGlobalVariable :: forall a. String -> a -> Effect Unit

renderSvg :: Effect (Promise String)
renderSvg = Promise.fromAff $ pure "<svg><text>Hello World!</text></svg>"

main :: Effect Unit
main = setGlobalVariable "$kissvg" {renderSvg}
