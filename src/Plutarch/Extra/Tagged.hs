{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Needed for Tagged instances for PlutusTx stuff
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.Tagged (
    PTagged (..),
    pretag,
) where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import GHC.Generics (Generic)
import Generics.SOP (Top)
import Plutarch (
    DPTStrat,
    DerivePlutusType,
    PlutusType,
    PlutusTypeNewtype,
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
 )
import Plutarch.Bool (PEq, POrd, PPartialOrd)
import Plutarch.Builtin (PAsData, PData, PIsData)
import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Boring (PBoring (pboring))
import Plutarch.Extra.Comonad (
    PComonad (pextract),
    PExtend (pextend),
 )
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap))
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Extra.Traversable (
    PSemiTraversable (psemitraverse, psemitraverse_),
    PTraversable (ptraverse, ptraverse_),
 )
import Plutarch.Integer (PIntegral)
import Plutarch.Lift (
    PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
    PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum (..))
import Plutarch.Show (PShow)
import Plutarch.TryFrom (PSubtype, PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import qualified PlutusTx

{- | Plutarch-level 'Tagged'. @PTagged@ allows one to "tag" a given type at the type level.
 Typically, @tag@ will be a 'Symbol', or some other custom kind. Since @PTagged@s only act
 as wrappers, their 'tags' don't hold any significance in the resulting script, and have no
 effect on performance or script size.

=== Tagged Types:
@
-- We can create a new kind by defining an empty type. This is then 'promoted' by GHC to a kind.
data Ada
data GT

PTagged Ada PInteger
PTagged GT PValue

-- Or we can use 'Symbol' (a type-level string)
PTagged "hello" PInteger
@

=== Tagging:
@
a :: Term s PInteger
a = pconstant 10

tagged_a1 :: Term s (PTagged "hello" PInteger)
tagged_a1 = ppure # a

tagged_a2 :: Term s (PTagged "hello" PInteger)
tagged_a2 = pcon . PTagged $ a

-- Since PTagged also implement Num typeclass, this is also possible.
-- Note, it requires the underlaying type to implement Num typeclass.
tagged_a3 :: Term s (PTagged "hello" PInteger)
tagged_a3 = 10

-- This is also possible. Note, it requires the underlaying type to
-- implement PLift typeclass.
tagged_a4 :: Term s (PTagged "hello" PInteger)
tagged_a4 = 10
@

== Untagging:
@
b :: Term s (PTagged "world" PInteger)
b = ppure # pconstant 5

tagged_b1 :: Term s PInteger
tagged_b1 = pextract # b

tagged_b2 :: Term s PInteger
tagged_b2 = pmatch b $ \case
    PTagged x -> x
@

 @since 1.0.0
-}
newtype PTagged (tag :: k) (underlying :: S -> Type) (s :: S)
    = PTagged (Term s underlying)
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd)

instance DerivePlutusType (PTagged k u) where type DPTStrat _ = PlutusTypeNewtype

-- | @since 1.0.0
deriving anyclass instance (PIntegral underlying) => PIntegral (PTagged tag underlying)

-- -- | @since 1.0.0
deriving anyclass instance (PNum underlying) => PNum (PTagged tag underlying)

-- -- | @since 1.0.0
deriving anyclass instance (PShow underlying) => PShow (PTagged tag underlying)

-- | @since 1.0.0
instance PFunctor (PTagged tag) where
    type PSubcategory (PTagged tag) = Top
    pfmap = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure . pcon . PTagged $ f # t'

-- | @since 1.0.0
instance PExtend (PTagged tag) where
    pextend = phoistAcyclic $ plam $ \f t -> pcon . PTagged $ f # t

-- | @since 1.0.0
instance PComonad (PTagged tag) where
    pextract = phoistAcyclic $
        plam $ \t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure t'

-- | @since 1.0.0
instance PApply (PTagged tag) where
    pliftA2 = phoistAcyclic $
        plam $ \f xs ys -> unTermCont $ do
            PTagged x <- pmatchC xs
            PTagged y <- pmatchC ys
            pure . pcon . PTagged $ f # x # y

-- | @since 1.0.0
instance PApplicative (PTagged tag) where
    ppure = phoistAcyclic $ plam $ pcon . PTagged

-- | @since 1.0.0
instance PTraversable (PTagged tag) where
    ptraverse = psemitraverse
    ptraverse_ = psemitraverse_

-- | @since 1.2.0
instance PSemiTraversable (PTagged tag) where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure $ pfmap # plam (pcon . PTagged) # (f # t')
    psemitraverse_ = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure $ f # t'

-- | @since 1.0.0
instance
    (PUnsafeLiftDecl a) =>
    PUnsafeLiftDecl (PTagged t a)
    where
    type PLifted (PTagged t a) = Tagged t (PLifted a)

-- | @since 1.0.0
instance (PConstantDecl a) => PConstantDecl (Tagged t a) where
    type PConstantRepr (Tagged t a) = PConstantRepr a
    type PConstanted (Tagged t a) = PTagged t (PConstanted a)
    pconstantToRepr (Tagged x) = pconstantToRepr x
    pconstantFromRepr x = Tagged <$> pconstantFromRepr x

{- | Hint: it's @punsafeCoerce@. :D

 @since 1.0.0
-}
pretag ::
    forall k' k.
    forall (tag' :: k') (tag :: k) (a :: S -> Type) (s :: S).
    Term s (PTagged tag a) ->
    Term s (PTagged tag' a)
pretag = punsafeCoerce

-- | @since 1.0.0
instance
    (PTryFrom PData (PAsData underlying), PSubtype PData (PAsData (PTagged tag underlying))) =>
    PTryFrom PData (PAsData (PTagged tag underlying))
    where
    type
        PTryFromExcess PData (PAsData (PTagged tag underlying)) =
            PTryFromExcess PData (PAsData underlying)
    ptryFrom' d k = ptryFrom' @_ @(PAsData underlying) d $ k . first punsafeCoerce

-- These are needed, because plutus-tx doesn't have them

-- | @since 1.0.0
deriving newtype instance
    (PlutusTx.ToData underlying) =>
    PlutusTx.ToData (Tagged tag underlying)

-- | @since 1.0.0
deriving newtype instance
    (PlutusTx.FromData underlying) =>
    PlutusTx.FromData (Tagged tag underlying)

-- | @since 1.0.0
deriving newtype instance
    (PlutusTx.UnsafeFromData underlying) =>
    PlutusTx.UnsafeFromData (Tagged tag underlying)

-- | @since 1.2.0
instance (PBoring underlying) => PBoring (PTagged tag underlying) where
    pboring = ppure # pboring
