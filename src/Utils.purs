module Utils where

import Prelude

import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags)
import Partial.Unsafe (unsafePartial)

whileM :: forall a m. Monad m => m Boolean -> m a -> m (Array a)
whileM p f = loop mempty
  where
    loop acc =
      p >>= if _
            then f >>= \x -> loop (acc <> pure x)
            else pure acc

orM :: forall m. Monad m => m Boolean -> m Boolean -> m Boolean
orM m1 m2 = m1 >>= case _ of
  true -> pure true
  false -> m2

parseFloat :: String -> Maybe Number
parseFloat = parseFloatImpl Nothing Just

regex :: String -> RegexFlags -> Regex
regex r flags = unsafePartial $ fromRight $ Regex.regex r flags

foreign import parseFloatImpl :: Maybe Number -> (Number -> Maybe Number) -> String -> Maybe Number


