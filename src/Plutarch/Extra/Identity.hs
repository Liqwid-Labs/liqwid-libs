{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Identity (
    PIdentity (..),
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
import Plutarch.Extra.Boring (PBoring (pboring))
import Plutarch.Extra.Comonad (
    PComonad (pextract),
    PExtend (pextend),
 )
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap))
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Integer (PIntegral)
import Plutarch.Show (PShow)

{- | Just a value of type @a@.

 @since 1.0.0
-}
newtype PIdentity (a :: S -> Type) (s :: S)
    = PIdentity (Term s a)

deriveGeneric ''PIdentity

-- | @since 1.0.0
deriving via (DerivePNewtype (PIdentity a) a) instance (PlutusType (PIdentity a))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PIdentity a) a)
    instance
        (PIsData a) => (PIsData (PIdentity a))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PIdentity a) a)
    instance
        (PEq a) => PEq (PIdentity a)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PIdentity a) a)
    instance
        (POrd a) => POrd (PIdentity a)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PIdentity a) a)
    instance
        (PIntegral a) => PIntegral (PIdentity a)

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PIdentity a) a))
    instance
        (Num (Term s a)) => Num (Term s (PIdentity a))

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PIdentity a) a))
    instance
        (Semigroup (Term s a)) => Semigroup (Term s (PIdentity a))

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PIdentity a) a))
    instance
        (Monoid (Term s a)) => Monoid (Term s (PIdentity a))

-- | @since 1.0.0
deriving anyclass instance (PShow a) => PShow (PIdentity a)

-- | @since 1.0.0
instance PFunctor PIdentity where
    type PSubcategory PIdentity = Top
    pfmap = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PIdentity t' <- pmatchC t
            pure . pcon . PIdentity $ f # t'

-- | @since 1.0.0
instance PExtend PIdentity where
    pextend = phoistAcyclic $ plam $ \f t -> pcon . PIdentity $ f # t

-- | @since 1.0.0
instance PComonad PIdentity where
    pextract = phoistAcyclic $
        plam $ \t -> unTermCont $ do
            PIdentity t' <- pmatchC t
            pure t'

-- | @since 1.0.0
instance PApply PIdentity where
    pliftA2 = phoistAcyclic $
        plam $ \f xs ys -> unTermCont $ do
            PIdentity tx <- pmatchC xs
            PIdentity ty <- pmatchC ys
            pure . pcon . PIdentity $ f # tx # ty

-- | @since 1.0.0
instance PApplicative PIdentity where
    ppure = phoistAcyclic $ plam $ pcon . PIdentity

-- | @since 1.2.0
instance (PBoring a) => PBoring (PIdentity a) where
    pboring = ppure # pboring
