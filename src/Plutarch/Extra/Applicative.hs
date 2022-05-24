{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Applicative (
    -- * Type classes
    PApply (..),
    PApplicative (..),

    -- * Functions
    (#<*>),
    (#*>),
    (#<*),
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
import Plutarch.Extra.Function (papply, pconst)
import Plutarch.Extra.Functor (PFunctor (PCovariantable))
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))

-- | @since 1.0.0
class (PFunctor f) => PApply (f :: (S -> Type) -> S -> Type) where
    pliftA2 ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
        (PCovariantable f a, PCovariantable f b, PCovariantable f c) =>
        Term s ((a :--> b :--> c) :--> f a :--> f b :--> f c)

-- | @since 1.0.0
instance PApply PMaybe where
    pliftA2 = phoistAcyclic $
        plam $ \f xs ys -> unTermCont $ do
            xs' <- pmatchC xs
            ys' <- pmatchC ys
            pure . pcon $ case (xs', ys') of
                (PJust x, PJust y) -> PJust $ f # x # y
                _ -> PNothing

-- TODO: PList, PBuiltinList, because it's a head-scratcher

-- | @since 1.0.0
instance (forall (s :: S). Semigroup (Term s a)) => PApply (PPair a) where
    pliftA2 = phoistAcyclic $
        plam $ \f xs ys -> unTermCont $ do
            PPair x1 x2 <- pmatchC xs
            PPair y1 y2 <- pmatchC ys
            pure . pcon . PPair (x1 <> y1) $ f # x2 # y2

{- | Forwards the /first/ 'PLeft'.

 @since 1.0.0
-}
instance PApply (PEither e) where
    pliftA2 = phoistAcyclic $
        plam $ \f xs ys -> unTermCont $ do
            xs' <- pmatchC xs
            ys' <- pmatchC ys
            pure . pcon $ case (xs', ys') of
                (PLeft e, _) -> PLeft e
                (_, PLeft e) -> PLeft e
                (PRight x, PRight y) -> PRight $ f # x # y

-- | @since 1.0.0
class (PApply f) => PApplicative (f :: (S -> Type) -> S -> Type) where
    ppure ::
        forall (a :: S -> Type) (s :: S).
        (PCovariantable f a) =>
        Term s (a :--> f a)

-- | @since 1.0.0
instance PApplicative PMaybe where
    ppure = phoistAcyclic $ plam $ pcon . PJust

-- TODO: PList, PBuiltinList due to Apply

-- | @since 1.0.0
instance (forall (s :: S). Monoid (Term s a)) => PApplicative (PPair a) where
    ppure = phoistAcyclic $ plam $ pcon . PPair mempty

-- | @since 1.0.0
instance PApplicative (PEither e) where
    ppure = phoistAcyclic $ plam $ pcon . PRight

-- | @since 1.0.0
(#<*>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PCovariantable f (a :--> b), PCovariantable f a, PCovariantable f b, PApply f) =>
    Term s (f (a :--> b)) ->
    Term s (f a) ->
    Term s (f b)
fs #<*> xs = pliftA2 # papply # fs # xs

-- | @since 1.0.0
(#*>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PCovariantable f a, PCovariantable f b, PApply f) =>
    Term s (f a) ->
    Term s (f b) ->
    Term s (f b)
t #*> t' = pliftA2 # plam (\_ x -> x) # t # t'

-- | @since 1.0.0
(#<*) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PCovariantable f a, PCovariantable f b, PApply f) =>
    Term s (f a) ->
    Term s (f b) ->
    Term s (f a)
t #<* t' = pliftA2 # pconst # t # t'
