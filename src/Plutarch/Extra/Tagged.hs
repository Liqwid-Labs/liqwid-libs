{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Tagged (
    PTagged (..),
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
import Plutarch.Extra.Comonad (PComonad (pextend, pextract))
import Plutarch.Extra.Functor (
    PBifunctor (PSubcategoryLeft, PSubcategoryRight, pbimap, psecond),
    PFunctor (PSubcategory, pfmap),
 )
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Extra.Traversable (PTraversable (ptraverse))
import Plutarch.Integer (PIntegral)
import Plutarch.Show (PShow)

{- | Plutarch-level 'Tagged'.

 @since 1.0.0
-}
newtype PTagged (tag :: S -> Type) (underlying :: S -> Type) (s :: S)
    = PTagged (Term s underlying)

deriveGeneric ''PTagged

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PTagged tag underlying) underlying)
    instance
        (PlutusType (PTagged tag underlying))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PTagged tag underlying) underlying)
    instance
        (PIsData underlying) => (PIsData (PTagged tag underlying))

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PTagged tag underlying) underlying)
    instance
        (PEq underlying) => PEq (PTagged tag underlying)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PTagged tag underlying) underlying)
    instance
        (POrd underlying) => POrd (PTagged tag underlying)

-- | @since 1.0.0
deriving via
    (DerivePNewtype (PTagged tag underlying) underlying)
    instance
        (PIntegral underlying) => PIntegral (PTagged tag underlying)

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PTagged tag underlying) underlying))
    instance
        (Num (Term s underlying)) => Num (Term s (PTagged tag underlying))

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PTagged tag underlying) underlying))
    instance
        (Semigroup (Term s underlying)) => Semigroup (Term s (PTagged tag underlying))

-- | @since 1.0.0
deriving via
    (Term s (DerivePNewtype (PTagged tag underlying) underlying))
    instance
        (Monoid (Term s underlying)) => Monoid (Term s (PTagged tag underlying))

-- | @since 1.0.0
deriving anyclass instance (PShow underlying) => PShow (PTagged tag underlying)

-- | @since 1.0.0
instance PFunctor (PTagged tag) where
    type PSubcategory (PTagged tag) = Top
    pfmap = psecond

-- | @since 1.0.0
instance PBifunctor PTagged where
    type PSubcategoryLeft PTagged = Top
    type PSubcategoryRight PTagged = Top
    pbimap = phoistAcyclic $
        plam $ \_ g t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure . pcon . PTagged $ g # t'

-- | @since 1.0.0
instance PComonad (PTagged tag) where
    pextract = phoistAcyclic $
        plam $ \t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure t'
    pextend = phoistAcyclic $ plam $ \f t -> pcon . PTagged $ f # t

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
    ptraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PTagged t' <- pmatchC t
            pure $ pfmap # plam (pcon . PTagged) # (f # t')
