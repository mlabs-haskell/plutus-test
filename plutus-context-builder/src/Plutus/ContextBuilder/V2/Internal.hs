module Plutus.ContextBuilder.V2.Internal (
  Normalizer (..),
  mkNormalized,
) where

import Data.Kind (Type)
import Plutus.ContextBuilder.V2.Base (BaseBuilder, Builder, mkNormalizedBase)

class (Builder a) => Normalizer (a :: Type) where
  mkNormalized' :: a -> a

instance Normalizer BaseBuilder where
  mkNormalized' = mkNormalizedBase

{- | Normalizes every value present in the builder structure.

 @since 2.4.0
-}
mkNormalized :: (Normalizer a) => a -> a
mkNormalized = mkNormalized'
