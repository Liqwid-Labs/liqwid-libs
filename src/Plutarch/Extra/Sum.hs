{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Sum (
    PSum (..),
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
import Plutarch.Extra.Functor (PFunctor (PCovariantable, pfmap))
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Integer (PIntegral)
import Plutarch.Show (PShow)

{- | A \'numerical\' value which is monoidal over its addition.

 @since 1.0.0
-}
newtype PSum (a :: S -> Type) (s :: S)
    = PSum (Term s a)

deriveGeneric ''PSum

-- | @since 1.0.0
deriving via (DerivePNewtype (PSum a) a) instance (PlutusType (PSum a))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PSum a) a)
    instance
        (PIsData a) => (PIsData (PSum a))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PSum a) a)
    instance
        (PEq a) => PEq (PSum a)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PSum a) a)
    instance
        (POrd a) => POrd (PSum a)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PSum a) a)
    instance
        (PIntegral a) => PIntegral (PSum a)

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PSum a) a))
    instance
        (Num (Term s a)) => Num (Term s (PSum a))

-- | @since 1.0.0
instance
    (forall (s' :: S). Num (Term s' a)) =>
    Semigroup (Term s (PSum a))
    where
    t <> t' = unTermCont $ do
        PSum x <- pmatchC t
        PSum y <- pmatchC t'
        pure . pcon . PSum $ x + y

-- | @since 1.0.0
instance
    (forall (s' :: S). Num (Term s' a)) =>
    Monoid (Term s (PSum a))
    where
    mempty = pcon . PSum $ 0

-- | @since 1.0.0
deriving anyclass instance (PShow a) => PShow (PSum a)

-- | @since 1.0.0
instance PFunctor PSum where
    type PCovariantable PSum = Top
    pfmap = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PSum t' <- pmatchC t
            pure . pcon . PSum $ f # t'

-- | @since 1.0.0
instance PApply PSum where
    pliftA2 = phoistAcyclic $
        plam $ \f xs ys -> unTermCont $ do
            PSum tx <- pmatchC xs
            PSum ty <- pmatchC ys
            pure . pcon . PSum $ f # tx # ty

-- | @since 1.0.0
instance PApplicative PSum where
    ppure = phoistAcyclic $ plam $ pcon . PSum
