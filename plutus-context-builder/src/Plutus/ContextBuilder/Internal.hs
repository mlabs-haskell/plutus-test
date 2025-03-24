module Plutus.ContextBuilder.Internal (
  Normalizer (..),
  mkNormalized,
) where

import Data.Kind (Type)
import Plutus.ContextBuilder.Base (BaseBuilder, Builder, mkNormalizedBase)

class (Builder a) => Normalizer (a :: Type) where
  mkNormalized' :: a -> a

instance Normalizer BaseBuilder where
  mkNormalized' = mkNormalizedBase

{- | Normalizes every value present in the builder structure.

 @since WIP
-}
mkNormalized :: (Normalizer a) => a -> a
mkNormalized = mkNormalized'
