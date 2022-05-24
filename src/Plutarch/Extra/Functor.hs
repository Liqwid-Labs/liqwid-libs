{-# LANGUAGE TypeFamilies #-}

module Plutarch.Extra.Functor (
    -- * Type classes
    PFunctor (..),
    PBifunctor (..),

    -- * Functions
    (#<$),
    (#$>),
    (#<$>),
    (#<&>),
    pvoid,
) where

import Data.Kind (Constraint, Type)
import Generics.SOP (Top)
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
import Plutarch.Builtin (PBuiltinList)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Extra.Function (pconst, pidentity)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Lift (PLift)
import Plutarch.List (PList, pmap)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unit (PUnit (PUnit))

-- | @since 1.0.0
class PFunctor (f :: (S -> Type) -> S -> Type) where
    type PCovariantable f :: (S -> Type) -> Constraint
    pfmap ::
        forall (a :: S -> Type) (b :: S -> Type) (s :: S).
        (PCovariantable f a, PCovariantable f b) =>
        Term s ((a :--> b) :--> f a :--> f b)

-- | @since 1.0.0
instance PFunctor PMaybe where
    type PCovariantable PMaybe = Top
    pfmap = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure . pcon $ case t' of
                PNothing -> PNothing
                PJust t'' -> PJust $ f # t''

-- | @since 1.0.0
instance PFunctor PList where
    type PCovariantable PList = Top
    pfmap = phoistAcyclic $ plam $ \f t -> pmap # f # t

-- | @since 1.0.0
instance PFunctor PBuiltinList where
    type PCovariantable PBuiltinList = PLift
    pfmap = phoistAcyclic $ plam $ \f t -> pmap # f # t

-- | @since 1.0.0
instance PFunctor (PPair a) where
    type PCovariantable (PPair a) = Top
    pfmap = psecond

-- | @since 1.0.0
instance PFunctor (PEither e) where
    type PCovariantable (PEither e) = Top
    pfmap = psecond

-- | @since 1.0.0
(#<$) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PCovariantable f a, PCovariantable f b) =>
    Term s a ->
    Term s (f b) ->
    Term s (f a)
x #<$ f = pfmap # (pconst # x) # f

-- | @since 1.0.0
(#$>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PCovariantable f a, PCovariantable f b) =>
    Term s (f a) ->
    Term s b ->
    Term s (f b)
(#$>) = flip (#<$)

-- | @since 1.0.0
(#<$>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PCovariantable f a, PCovariantable f b) =>
    Term s (a :--> b) ->
    Term s (f a) ->
    Term s (f b)
f #<$> t = pfmap # f # t

-- | @since 1.0.0
(#<&>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PCovariantable f a, PCovariantable f b) =>
    Term s (f a) ->
    Term s (a :--> b) ->
    Term s (f b)
(#<&>) = flip (#<$>)

-- | @since 1.0.0
pvoid ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PFunctor f, PCovariantable f a, PCovariantable f PUnit) =>
    Term s (f a) ->
    Term s (f PUnit)
pvoid t = t #$> pcon PUnit

-- | @since 1.0.0
class PBifunctor (f :: (S -> Type) -> (S -> Type) -> S -> Type) where
    type PBicovariantableLeft f :: (S -> Type) -> Constraint
    type PBicovariantableRight f :: (S -> Type) -> Constraint
    {-# MINIMAL pbimap | pfirst, psecond #-}
    pbimap ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type) (s :: S).
        ( PBicovariantableLeft f a
        , PBicovariantableLeft f c
        , PBicovariantableRight f b
        , PBicovariantableRight f d
        ) =>
        Term s ((a :--> c) :--> (b :--> d) :--> f a b :--> f c d)
    pbimap = phoistAcyclic $ plam $ \f g t -> pfirst # f # (psecond # g # t)
    pfirst ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
        ( PBicovariantableLeft f a
        , PBicovariantableLeft f c
        , PBicovariantableRight f b
        ) =>
        Term s ((a :--> c) :--> f a b :--> f c b)
    pfirst = phoistAcyclic $ plam $ \f t -> pbimap # f # pidentity # t
    psecond ::
        forall (a :: S -> Type) (b :: S -> Type) (d :: S -> Type) (s :: S).
        ( PBicovariantableLeft f a
        , PBicovariantableRight f b
        , PBicovariantableRight f d
        ) =>
        Term s ((b :--> d) :--> f a b :--> f a d)
    psecond = phoistAcyclic $ plam $ \g t -> pbimap # pidentity # g # t

-- | @since 1.0.0
instance PBifunctor PPair where
    type PBicovariantableLeft PPair = Top
    type PBicovariantableRight PPair = Top
    pbimap = phoistAcyclic $
        plam $ \f g t -> unTermCont $ do
            PPair x y <- pmatchC t
            pure . pcon . PPair (f # x) $ g # y

-- | @since 1.0.0
instance PBifunctor PEither where
    type PBicovariantableLeft PEither = Top
    type PBicovariantableRight PEither = Top
    pbimap = phoistAcyclic $
        plam $ \f g t -> unTermCont $ do
            t' <- pmatchC t
            pure . pcon $ case t' of
                PLeft x -> PLeft $ f # x
                PRight y -> PRight $ g # y
