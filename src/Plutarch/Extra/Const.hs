{-# LANGUAGE QuantifiedConstraints #-}
-- Needed to connect PConst to Const
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.Const (
  -- * Type
  PConst (..),

  -- * Helper functions
  preconst,
) where

import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Boring (PBoring (pboring))
import Plutarch.Extra.Functor (
  PBifunctor (PSubcategoryLeft, PSubcategoryRight, pbimap, psecond),
  PFunctor (PSubcategory, pfmap),
  Plut,
 )
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Num (PNum)
import Plutarch.Unsafe (punsafeCoerce)

{- | A value of type @a@ pretending to a be a value of type @b@.

 @since 1.0.0
-}
newtype PConst (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PConst (Term s a)
  deriving stock
    ( -- | @since 1.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.4.0
instance DerivePlutusType (PConst a b) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 1.0.0
deriving anyclass instance (PIsData a) => (PIsData (PConst a b))

-- | @since 1.0.0
deriving anyclass instance (PEq a) => PEq (PConst a b)

-- | @since 1.4.0
deriving anyclass instance (POrd a) => PPartialOrd (PConst a b)

-- | @since 1.0.0
deriving anyclass instance (POrd a) => POrd (PConst a b)

-- | @since 1.0.0
deriving anyclass instance (PIntegral a) => PIntegral (PConst a b)

-- | @since 1.4.0
deriving anyclass instance (PNum a) => PNum (PConst a b)

-- | @since 1.0.0
deriving anyclass instance (PShow a) => PShow (PConst a b)

-- | @since 3.1.0
instance PFunctor (PConst a) where
  type PSubcategory (PConst a) = Plut
  pfmap = psecond

-- | @since 3.1.0
instance PBifunctor PConst where
  type PSubcategoryLeft PConst = Plut
  type PSubcategoryRight PConst = Plut
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
