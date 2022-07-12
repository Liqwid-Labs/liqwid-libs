{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Const (
    -- * Type
    PConst (..),

    -- * Helper functions
    preconst,
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
import Plutarch.Extra.Functor (
    PBifunctor (PSubcategoryLeft, PSubcategoryRight, pbimap, psecond),
    PFunctor (PSubcategory, pfmap),
 )
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Integer (PIntegral)
import Plutarch.Show (PShow)
import Plutarch.Unsafe (punsafeCoerce)

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
    type PSubcategory (PConst a) = Top
    pfmap = psecond

-- | @since 1.0.0
instance PBifunctor PConst where
    type PSubcategoryLeft PConst = Top
    type PSubcategoryRight PConst = Top
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

-- | @since 1.2.0
instance (PBoring a) => PBoring (PConst a b) where
    pboring = pcon . PConst $ pboring

{- | Since 'PConst' is only /pretending/ to be a value of another type, we can
 change what we \'pretend to be\' without having to rebuild. Essentially, this
 is 'punsafeCoerce', but because we're only changing a tag, we're not worried.

 @since 1.2.0
-}
preconst ::
    forall (c :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s (PConst a b) ->
    Term s (PConst a c)
preconst = punsafeCoerce
