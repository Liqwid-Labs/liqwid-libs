{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Orphans () where

import Control.Composition (on, (.*))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

-- | @since 1.3.0
instance (Semigroup (Term s a), a ~ PInner b) => Semigroup (Term s b) where
    (<>) = punsafeDowncast .* ((<>) `on` punsafeCoerce)

-- | @since 1.3.0
instance (Monoid (Term s a), a ~ PInner b) => Monoid (Term s b) where
    mempty = punsafeDowncast mempty
