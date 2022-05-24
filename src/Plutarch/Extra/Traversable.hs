{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Traversable (
    -- * Type classes
    PTraversable (..),
    PSemiTraversable (..),

    -- * Functions

    -- ** Folds
    psemifold,
    psemifoldMap,
    pfold,
    pfoldMap,

    -- ** Specialized folds
    plength,
    psum,
) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Extra.Applicative (PApplicative (ppure), PApply)
import Plutarch.Extra.Const (PConst (PConst))
import Plutarch.Extra.Functor (PFunctor (PCovariantable, pfmap))
import Plutarch.Extra.Identity (PIdentity (PIdentity))
import Plutarch.Extra.Sum (PSum (PSum))
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unit (PUnit)

-- | @since 1.0.0
class (PFunctor t) => PTraversable (t :: (S -> Type) -> S -> Type) where
    ptraverse ::
        forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
        ( PApplicative f
        , PCovariantable f a
        , PCovariantable f b
        , PCovariantable f (t b)
        ) =>
        Term s ((a :--> f b) :--> t a :--> f (t b))

-- | @since 1.0.0
instance PTraversable PIdentity where
    ptraverse = psemitraverse

-- | @since 1.0.0
instance PTraversable PSum where
    ptraverse = psemitraverse

-- | @since 1.0.0
instance PTraversable (PConst a) where
    ptraverse = phoistAcyclic $
        plam $ \_ t -> unTermCont $ do
            PConst tx <- pmatchC t
            pure $ ppure # (pcon . PConst $ tx)

-- | @since 1.0.0
instance PTraversable PMaybe where
    ptraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            case t' of
                PNothing -> pure $ ppure # pcon PNothing
                PJust x -> do
                    res <- pletC (f # x)
                    pure $ pfmap # plam (pcon . PJust) # res

-- TODO: PList, PBuiltinList, because tedium

-- | @since 1.0.0
instance PTraversable (PPair a) where
    ptraverse = psemitraverse

-- | @since 1.0.0
instance PTraversable (PEither e) where
    ptraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            case t' of
                PLeft e -> pure $ ppure # (pcon . PLeft $ e)
                PRight x -> do
                    res <- pletC (f # x)
                    pure $ pfmap # plam (pcon . PRight) # res

-- | @since 1.0.0
class (PTraversable t) => PSemiTraversable (t :: (S -> Type) -> S -> Type) where
    psemitraverse ::
        forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
        ( PApply f
        , PCovariantable f a
        , PCovariantable f b
        , PCovariantable f (t b)
        ) =>
        Term s ((a :--> f b) :--> t a :--> f (t b))

-- | @since 1.0.0
instance PSemiTraversable PIdentity where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PIdentity tx <- pmatchC t
            pure $ pfmap # ppure # (f # tx)

-- | @since 1.0.0
instance PSemiTraversable PSum where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PSum tx <- pmatchC t
            pure $ pfmap # ppure # (f # tx)

-- | @since 1.0.0
instance PSemiTraversable (PPair a) where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PPair x y <- pmatchC t
            res <- pletC (f # y)
            pure $ pfmap # plam (pcon . PPair x) # res

{- | Collapse a non-empty \'structure\' full of a 'Semigroup'.

 @since 1.0.0
-}
psemifold ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PSemiTraversable t, forall (s' :: S). Semigroup (Term s' a)) =>
    Term s (t a :--> a)
psemifold = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        PConst res <- pmatchC (psemitraverse # go # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' (a :--> PConst a PUnit)
    go = phoistAcyclic $ plam $ pcon . PConst

{- | Collapse a non-empty \'structure\' with a projection into a 'Semigroup'.

 @since 1.0.0
-}
psemifoldMap ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PSemiTraversable t, forall (s' :: S). Semigroup (Term s' b)) =>
    Term s ((a :--> b) :--> t a :--> b)
psemifoldMap = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
        PConst res <- pmatchC (psemitraverse # (go # f) # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' ((a :--> b) :--> a :--> PConst b PUnit)
    go = phoistAcyclic $ plam $ \f x -> pcon . PConst $ f # x

{- | Collapse a possibly empty \'structure\' full of a 'Monoid'.

 @since 1.0.0
-}
pfold ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PTraversable t, forall (s' :: S). Monoid (Term s' a)) =>
    Term s (t a :--> a)
pfold = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        PConst res <- pmatchC (ptraverse # go # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' (a :--> PConst a PUnit)
    go = phoistAcyclic $ plam $ pcon . PConst

{- | Collapse a possibly empty \'structure\' with a projection into a 'Monoid'.

 @since 1.0.0
-}
pfoldMap ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PTraversable t, forall (s' :: S). Monoid (Term s' b)) =>
    Term s ((a :--> b) :--> t a :--> b)
pfoldMap = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
        PConst res <- pmatchC (ptraverse # (go # f) # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' ((a :--> b) :--> a :--> PConst b PUnit)
    go = phoistAcyclic $ plam $ \f x -> pcon . PConst $ f # x

{- | Counts the number of elements in the \'structure\'.

 @since 1.0.0
-}
plength ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PTraversable t) =>
    Term s (t a :--> PInteger)
plength = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        PSum res <- pmatchC (pfoldMap # go # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' (a :--> PSum PInteger)
    go = phoistAcyclic $ plam $ \_ -> pcon . PSum $ 1

{- | \'Add up\' all the elements in the structure.

 @since 1.0.0
-}
psum ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PTraversable t, forall (s' :: S). Num (Term s' a)) =>
    Term s (t a :--> a)
psum = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        PSum res <- pmatchC (pfoldMap # go # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' (a :--> PSum a)
    go = phoistAcyclic $ plam $ pcon . PSum
