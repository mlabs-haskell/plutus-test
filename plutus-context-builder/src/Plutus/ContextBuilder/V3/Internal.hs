module Plutus.ContextBuilder.V3.Internal (
  Normalizer (..),
  mkNormalized,
)
where

import Data.Kind (Type)
import Plutus.ContextBuilder.V3.Base (BaseBuilder, Builder, mkNormalizedBase)

class (Builder a) => Normalizer (a :: Type) where
  mkNormalized' :: a -> a

instance Normalizer BaseBuilder where
  mkNormalized' = mkNormalizedBase

{- | Normalizes every value present in the builder structure.

@since 3.0.0
-}
mkNormalized :: (Normalizer a) => a -> a
mkNormalized = mkNormalized'
