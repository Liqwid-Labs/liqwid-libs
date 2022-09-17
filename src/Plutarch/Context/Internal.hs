module Plutarch.Context.Internal(
  Normalizer(..),
  mkNormalized,
 ) where

import Data.Kind (Type)
import Plutarch.Context.Base (Builder, BaseBuilder, mkNormalizedBase)

class Builder a => Normalizer (a :: Type) where
  mkNormalized' :: a -> a

instance Normalizer BaseBuilder where
  mkNormalized' = mkNormalizedBase

{- | Normalizes every value present in the builder structure. 

 @since 2.4.0
-}
mkNormalized :: Normalizer a => a -> a
mkNormalized = mkNormalized'
