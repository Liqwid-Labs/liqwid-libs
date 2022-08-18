{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Orphans () where

import Control.Composition (on, (.*))
import Plutarch.Api.V2 (PDatumHash (PDatumHash), PScriptHash (PScriptHash))
import Plutarch.Builtin (PIsData (..))
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.TryFrom (PTryFrom (..))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- | @since 1.3.0
instance {-# OVERLAPPABLE #-} (Semigroup (Term s a), a ~ PInner b) => Semigroup (Term s b) where
    (<>) = punsafeDowncast .* ((<>) `on` punsafeCoerce)

-- | @since 1.3.0
instance {-# OVERLAPPABLE #-} (Monoid (Term s a), a ~ PInner b) => Monoid (Term s b) where
    mempty = punsafeDowncast mempty

-- | @since 3.0.3
instance (PIsData a) => PIsData (PAsData a) where
    pfromDataImpl = punsafeCoerce
    pdataImpl = pdataImpl . pfromData

-- | @since 3.0.3
instance PTryFrom PData (PAsData PDatumHash) where
    type PTryFromExcess PData (PAsData PDatumHash) = Flip Term PDatumHash
    ptryFrom' opq = runTermCont $ do
        unwrapped <- pfromData . fst <$> ptryFromC @(PAsData PByteString) opq

        tcont $ \f ->
            pif
                -- Blake2b_256 hash: 256 bits/32 bytes.
                (plengthBS # unwrapped #== 32)
                (f ())
                (ptraceError "ptryFrom(PDatumHash): must be 32 bytes long")

        pure (punsafeCoerce opq, pcon $ PDatumHash unwrapped)

-- | @since 3.0.3
instance PTryFrom PData (PAsData PUnit)

-- | @since 3.0.3
instance PTryFrom PData (PAsData PScriptHash) where
    type PTryFromExcess PData (PAsData PScriptHash) = Flip Term PScriptHash
    ptryFrom' opq = runTermCont $ do
        unwrapped <- pfromData . fst <$> ptryFromC @(PAsData PByteString) opq

        tcont $ \f ->
            pif
                -- Blake2b_224 hash: 224 bits/28 bytes.
                (plengthBS # unwrapped #== 28)
                (f ())
                (ptraceError "ptryFrom(PScriptHash): must be 28 bytes long")

        pure (punsafeCoerce opq, pcon $ PScriptHash unwrapped)
