{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Const (
    PConst (..),
) where

import Data.Kind (Type)
import Generics.SOP (I (I), Top)
import Generics.SOP.TH (deriveGeneric)
import Plutarch (
    DerivePNewtype (DerivePNewtype),
    PlutusType,
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
 )
import Plutarch.Bool (PEq, POrd)
import Plutarch.Builtin (PIsData)
import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Functor (
    PBifunctor (PBicovariantableLeft, PBicovariantableRight, pbimap, psecond),
    PFunctor (PCovariantable, pfmap),
 )
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Integer (PIntegral)
import Plutarch.Show (PShow)

{- | A value of type @a@ pretending to a be a value of type @b@.

 @since 1.0.0
-}
newtype PConst (a :: S -> Type) (b :: S -> Type) (s :: S)
    = PConst (Term s a)

deriveGeneric ''PConst

-- | @since 1.0.0
deriving via (DerivePNewtype (PConst a b) a) instance (PlutusType (PConst a b))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PConst a b) a)
    instance
        (PIsData a) => (PIsData (PConst a b))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PConst a b) a)
    instance
        (PEq a) => PEq (PConst a b)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PConst a b) a)
    instance
        (POrd a) => POrd (PConst a b)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PConst a b) a)
    instance
        (PIntegral a) => PIntegral (PConst a b)

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PConst a b) a))
    instance
        (Num (Term s a)) => Num (Term s (PConst a b))

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PConst a b) a))
    instance
        (Semigroup (Term s a)) => Semigroup (Term s (PConst a b))

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PConst a b) a))
    instance
        (Monoid (Term s a)) => Monoid (Term s (PConst a b))

-- | @since 1.0.0
deriving anyclass instance (PShow a) => PShow (PConst a b)

-- | @since 1.0.0
instance PFunctor (PConst a) where
    type PCovariantable (PConst a) = Top
    pfmap = psecond

-- | @since 1.0.0
instance PBifunctor PConst where
    type PBicovariantableLeft PConst = Top
    type PBicovariantableRight PConst = Top
    pbimap = phoistAcyclic $
        plam $ \f _ t -> unTermCont $ do
            PConst tx <- pmatchC t
            pure . pcon . PConst $ f # tx

-- | @since 1.0.0
instance
    (forall (s :: S). Semigroup (Term s a)) =>
    PApply (PConst a)
    where
    pliftA2 = phoistAcyclic $
        plam $ \_ xs ys -> unTermCont $ do
            PConst tx <- pmatchC xs
            PConst ty <- pmatchC ys
            pure . pcon . PConst $ tx <> ty

-- | @since 1.0.0
instance
    (forall (s :: S). Monoid (Term s a)) =>
    PApplicative (PConst a)
    where
    ppure = phoistAcyclic $ plam $ \_ -> pcon . PConst $ mempty
