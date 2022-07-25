{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.Bind (
    -- * Type class
    PBind (..),

    -- * Functions
    (#>>=),
    pjoin,
) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    pcon,
    pfix,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust, PDNothing))
import Plutarch.Builtin (PBuiltinList, pfromData)
import Plutarch.DataRepr (pfield)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Extra.Applicative (PApply)
import Plutarch.Extra.Function (pidentity)
import Plutarch.Extra.Functor (PSubcategory)
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.List (PList, pconcat, pnil, puncons)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))

{- | Gives the capability to bind a Kleisli arrow over @f@ to a value:
 essentially, the equivalent of Haskell's '>>='. Unlike Haskell, we don't
 require the availability of 'pure': to recover the equivalent of Haskell's
 'Monad', you want both 'PApplicative' and 'PBind'.

 = Laws

 * @'pbind' '#' ('pbind' '#' m '#' f) '#' g@ @=@
 @'pbind' '#' m '#' ('plam' '$' \x -> 'pbind' '#' (f '#' x) '#' g)@
 * @'pbind' '#' f '#' ('plam' '$' \g -> 'pfmap' '#' g '#' x)@ @=@
 @'pliftA2' '#' 'papply' '#' f '#' x@

 @since 1.2.1
-}
class (PApply f) => PBind (f :: (S -> Type) -> S -> Type) where
    -- | '>>=', but as a Plutarch function.
    pbind ::
        forall (a :: S -> Type) (b :: S -> Type) (s :: S).
        (PSubcategory f a, PSubcategory f b) =>
        Term s (f a :--> (a :--> f b) :--> f b)

-- | @since 1.2.1
instance PBind PMaybe where
    pbind = phoistAcyclic $
        plam $ \xs f -> unTermCont $ do
            xs' <- pmatchC xs
            pure $ case xs' of
                PNothing -> pcon PNothing
                PJust t -> f # t

-- | @since 1.2.1
instance PBind PMaybeData where
    pbind = phoistAcyclic $
        plam $ \xs f -> unTermCont $ do
            xs' <- pmatchC xs
            pure $ case xs' of
                PDNothing t -> pcon . PDNothing $ t
                PDJust t -> f # pfromData (pfield @"_0" # t)

-- | @since 1.2.1
instance PBind PList where
    pbind = phoistAcyclic $
        pfix #$ plam $ \self xs f -> unTermCont $ do
            xs' <- pmatchC (puncons # xs)
            case xs' of
                PNothing -> pure pnil
                PJust t -> do
                    PPair h rest <- pmatchC t
                    pure $ pconcat # (f # h) # (self # rest # f)

-- | @since 1.2.1
instance PBind PBuiltinList where
    pbind = phoistAcyclic $
        pfix #$ plam $ \self xs f -> unTermCont $ do
            xs' <- pmatchC (puncons # xs)
            case xs' of
                PNothing -> pure pnil
                PJust t -> do
                    PPair h rest <- pmatchC t
                    pure $ pconcat # (f # h) # (self # rest # f)

-- | @since 1.2.1
instance (forall (s :: S). Semigroup (Term s a)) => PBind (PPair a) where
    pbind = phoistAcyclic $
        plam $ \xs f -> unTermCont $ do
            PPair acc t <- pmatchC xs
            PPair acc' res <- pmatchC (f # t)
            pure . pcon . PPair (acc <> acc') $ res

{- | Forwards the /first/ 'PLeft'.

 @since 1.2.1
-}
instance PBind (PEither e) where
    pbind = phoistAcyclic $
        plam $ \xs f -> unTermCont $ do
            t <- pmatchC xs
            pure $ case t of
                PLeft t' -> pcon . PLeft $ t'
                PRight t' -> f # t'

{- | Infix form of 'pbind'.

 @since 1.2.1
-}
(#>>=) ::
    forall (a :: S -> Type) (b :: S -> Type) (f :: (S -> Type) -> S -> Type) (s :: S).
    (PBind f, PSubcategory f a, PSubcategory f b) =>
    Term s (f a) ->
    Term s (a :--> f b) ->
    Term s (f b)
xs #>>= f = pbind # xs # f

{- | \'Flattens\' two identical 'PBind' layers into one.

 @since 1.2.1
-}
pjoin ::
    forall (a :: S -> Type) (f :: (S -> Type) -> S -> Type) (s :: S).
    (PBind f, PSubcategory f a, PSubcategory f (f a)) =>
    Term s (f (f a) :--> f a)
pjoin = phoistAcyclic $ plam $ \xs -> pbind # xs # pidentity
