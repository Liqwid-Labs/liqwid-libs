{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Sum (
  PSum (..),
) where

import Plutarch.Extra.Applicative (PApplicative (ppure), PApply (pliftA2))
import Plutarch.Extra.Boring (PBoring (pboring))
import Plutarch.Extra.Comonad (PComonad (pextract), PExtend (pextend))
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap), Plut)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Num (PNum)

{- | A \'numerical\' value which is monoidal over its addition.

 @since 1.0.0
-}
newtype PSum (a :: S -> Type) (s :: S)
  = PSum (Term s a)
  deriving stock
    ( -- | @since 1.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.0.0
      PlutusType
    )

-- | @since 1.4.0
instance DerivePlutusType (PSum a) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since 1.0.0
deriving anyclass instance (PIsData a) => (PIsData (PSum a))

-- | @since 1.0.0
deriving anyclass instance (PEq a) => PEq (PSum a)

-- | @since 1.4.0
deriving anyclass instance (POrd a) => PPartialOrd (PSum a)

-- | @since 1.0.0
deriving anyclass instance (POrd a) => POrd (PSum a)

-- | @since 1.0.0
deriving anyclass instance (PIntegral a) => PIntegral (PSum a)

-- | @since 1.0.0
deriving anyclass instance (PNum a) => PNum (PSum a)

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

-- | @since 3.1.0
instance PFunctor PSum where
  type PSubcategory PSum = Plut
  pfmap = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
      PSum t' <- pmatchC t
      pure . pcon . PSum $ f # t'

-- | @since 1.0.0
instance PExtend PSum where
  pextend = phoistAcyclic $ plam $ \f t -> pcon . PSum $ f # t

-- | @since 1.0.0
instance PComonad PSum where
  pextract = phoistAcyclic $
    plam $ \t -> unTermCont $ do
      PSum t' <- pmatchC t
      pure t'

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

-- | @since 1.2.0
instance (PBoring a) => PBoring (PSum a) where
  pboring = ppure # pboring
