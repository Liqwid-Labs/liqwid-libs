{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.Traversable (
    -- * Type classes
    PTraversable (..),
    PSemiTraversable (..),

    -- * Functions

    -- ** Folds
    psemifold,
    psemifoldMap,
    psemifoldComonad,
    pfold,
    pfoldMap,
    pfoldComonad,

    -- ** Specialized folds
    plength,
    psum,
) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    pcon,
    pfix,
    phoistAcyclic,
    plam,
    pmatch,
    unTermCont,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust, PDNothing))
import Plutarch.Builtin (PBuiltinList, pdata, pfromData)
import Plutarch.DataRepr (pdcons, pdnil, pfield)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Extra.Applicative (
    PApplicative (ppure),
    PApply (pliftA2),
    (#*>),
 )
import Plutarch.Extra.Boring (PBoring (pboring))
import Plutarch.Extra.Comonad (PComonad (pextract))
import Plutarch.Extra.Const (PConst (PConst), preconst)
import Plutarch.Extra.Functor (PFunctor (PSubcategory, pfmap))
import Plutarch.Extra.Identity (PIdentity (PIdentity))
import Plutarch.Extra.Sum (PSum (PSum))
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Integer (PInteger)
import Plutarch.List (PList, pcons, pnil, puncons)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unit (PUnit)

-- | @since 1.0.0
class (PFunctor t) => PTraversable (t :: (S -> Type) -> S -> Type) where
    ptraverse ::
        forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
        ( PApplicative f
        , PSubcategory f a
        , PSubcategory f b
        , PSubcategory f (t b)
        , PSubcategory t a
        , PSubcategory t b
        ) =>
        Term s ((a :--> f b) :--> t a :--> f (t b))

    -- | This avoids re-building the input 'PTraversable' if we end up throwing
    -- it away anyway. In the case where we only care about the effect, and
    -- don't need the 'PTraversable' afterwards, this can be more efficient.
    --
    -- = Note
    --
    -- This is \'boredom-polymorphic\' to ensure that we don't run into issues
    -- with 'PSubcategory' constraints. This is why we choose the order of type
    -- variables as we do: it allows you to easily choose which 'PBoring' thing
    -- you want.
    --
    -- @since 1.2.0
    ptraverse_ ::
        forall
            (b :: S -> Type)
            (f :: (S -> Type) -> S -> Type)
            (a :: S -> Type)
            (s :: S).
        ( PApplicative f
        , PSubcategory f b
        , PBoring b
        , PSubcategory t a
        ) =>
        Term s ((a :--> f b) :--> t a :--> f b)

-- | @since 1.0.0
instance PTraversable PIdentity where
    ptraverse = psemitraverse
    ptraverse_ = psemitraverse_

-- | @since 1.0.0
instance PTraversable PSum where
    ptraverse = psemitraverse
    ptraverse_ = psemitraverse_

-- | @since 1.0.0
instance PTraversable (PConst a) where
    ptraverse = phoistAcyclic $
        plam $ \_ t ->
            ppure # preconst t
    ptraverse_ = phoistAcyclic $
        plam $ \_ _ ->
            ppure # pboring

-- | @since 1.0.0
instance PTraversable PMaybe where
    ptraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure $ case t' of
                PNothing -> ppure # pcon PNothing
                PJust x -> pfmap # plam (pcon . PJust) # (f # x)
    ptraverse_ = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure $ case t' of
                PNothing -> ppure # pboring
                PJust x -> f # x

-- | @since 1.0.0
instance PTraversable PMaybeData where
    ptraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure $ case t' of
                PDNothing _ -> ppure # (pcon . PDNothing $ pdnil)
                PDJust t'' ->
                    let res = f # pfromData (pfield @"_0" # t'')
                     in pfmap # plam (\y -> pcon . PDJust $ pdcons # pdata y # pdnil) # res
    ptraverse_ = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure $ case t' of
                PDNothing _ -> ppure # pboring
                PDJust t'' -> f # pfromData (pfield @"_0" # t'')

-- | @since 1.0.0
instance PTraversable PList where
    ptraverse = phoistAcyclic $
        pfix
            #$ plam
            $ \self f xs ->
                pmatch (puncons # xs) $ \case
                    PNothing -> ppure # pnil
                    PJust t' -> do
                        pmatch t' $ \case
                            PPair thead ttail ->
                                pliftA2 # pcons # (f # thead) # (self # f # ttail)
    ptraverse_ ::
        forall
            (b :: S -> Type)
            (f :: (S -> Type) -> S -> Type)
            (a :: S -> Type)
            (s :: S).
        ( PApplicative f
        , PSubcategory f b
        , PBoring b
        ) =>
        Term s ((a :--> f b) :--> PList a :--> f b)
    ptraverse_ = phoistAcyclic $ pfix #$ plam $ go
      where
        go ::
            forall (s' :: S).
            Term s' ((a :--> f b) :--> PList a :--> f b) ->
            Term s' (a :--> f b) ->
            Term s' (PList a) ->
            Term s' (f b)
        go self f xs = unTermCont $ do
            t <- pmatchC (puncons # xs)
            case t of
                PNothing -> pure $ ppure # pboring
                PJust t' -> do
                    PPair thead ttail <- pmatchC t'
                    pure $ (f # thead) #*> (self # f # ttail)

-- | @since 1.0.0
instance PTraversable PBuiltinList where
    ptraverse = phoistAcyclic $
        pfix
            #$ plam
            $ \r f xs ->
                pmatch (puncons # xs) $ \case
                    PNothing -> ppure # pnil
                    PJust t' -> do
                        pmatch t' $ \case
                            PPair thead ttail ->
                                pliftA2 # pcons # (f # thead) # (r # f # ttail)
    ptraverse_ ::
        forall
            (b :: S -> Type)
            (f :: (S -> Type) -> S -> Type)
            (a :: S -> Type)
            (s :: S).
        ( PApplicative f
        , PSubcategory f b
        , PBoring b
        , PSubcategory PBuiltinList a
        ) =>
        Term s ((a :--> f b) :--> PBuiltinList a :--> f b)
    ptraverse_ = phoistAcyclic $ pfix #$ plam $ go
      where
        go ::
            forall (s' :: S).
            Term s' ((a :--> f b) :--> PBuiltinList a :--> f b) ->
            Term s' (a :--> f b) ->
            Term s' (PBuiltinList a) ->
            Term s' (f b)
        go self f xs = unTermCont $ do
            t <- pmatchC (puncons # xs)
            case t of
                PNothing -> pure $ ppure # pboring
                PJust t' -> do
                    PPair thead ttail <- pmatchC t'
                    pure $ (f # thead) #*> (self # f # ttail)

-- | @since 1.0.0
instance PTraversable (PPair a) where
    ptraverse = psemitraverse
    ptraverse_ = psemitraverse_

-- | @since 1.0.0
instance PTraversable (PEither e) where
    ptraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure $ case t' of
                PLeft e -> ppure # (pcon . PLeft $ e)
                PRight x -> pfmap # plam (pcon . PRight) # (f # x)
    ptraverse_ = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure $ case t' of
                PLeft _ -> ppure # pboring
                PRight x -> f # x

-- | @since 1.0.0
class (PTraversable t) => PSemiTraversable (t :: (S -> Type) -> S -> Type) where
    psemitraverse ::
        forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
        ( PApply f
        , PSubcategory f a
        , PSubcategory f b
        , PSubcategory f (t b)
        ) =>
        Term s ((a :--> f b) :--> t a :--> f (t b))

    -- Similar to 'ptraverse_', but only requiring an 'Apply'.
    --
    -- @since 1.2.0
    psemitraverse_ ::
        forall
            (b :: S -> Type)
            (f :: (S -> Type) -> S -> Type)
            (a :: S -> Type)
            (s :: S).
        ( PApply f
        , PSubcategory f b
        , PBoring b
        , PSubcategory t a
        ) =>
        Term s ((a :--> f b) :--> t a :--> f b)

-- | @since 1.0.0
instance PSemiTraversable PIdentity where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PIdentity tx <- pmatchC t
            pure $ pfmap # ppure # (f # tx)
    psemitraverse_ = phoistAcyclic $ plam $ \f t -> f # (pextract # t)

-- | @since 1.0.0
instance PSemiTraversable PSum where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PSum tx <- pmatchC t
            pure $ pfmap # ppure # (f # tx)
    psemitraverse_ = phoistAcyclic $ plam $ \f t -> f # (pextract # t)

-- | @since 1.0.0
instance PSemiTraversable (PPair a) where
    psemitraverse = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PPair x y <- pmatchC t
            res <- pletC (f # y)
            pure $ pfmap # plam (pcon . PPair x) # res
    psemitraverse_ = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            PPair _ ty <- pmatchC t
            pure $ f # ty

{- | Collapse a non-empty \'structure\' full of a 'Semigroup'.

 @since 1.2.0
-}
psemifold ::
    forall
        (t :: (S -> Type) -> S -> Type)
        (a :: S -> Type)
        (s :: S).
    ( PSemiTraversable t
    , forall (s' :: S). Semigroup (Term s' a)
    , PSubcategory t a
    ) =>
    Term s (t a :--> a)
psemifold = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        PConst res <- pmatchC (psemitraverse_ # go # t)
        pure res
  where
    go :: forall (s' :: S). Term s' (a :--> PConst a PUnit)
    go = phoistAcyclic $ plam $ pcon . PConst

{- | Collapse a non-empty \'structure\' with a projection into a 'Semigroup'.

 @since 1.2.0
-}
psemifoldMap ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PSemiTraversable t, forall (s' :: S). Semigroup (Term s' b), PSubcategory t a) =>
    Term s ((a :--> b) :--> t a :--> b)
psemifoldMap = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
        PConst res <- pmatchC (psemitraverse_ # (go # f) # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' ((a :--> b) :--> a :--> PConst b PUnit)
    go = phoistAcyclic $ plam $ \f x -> pcon . PConst $ f # x

{- | Collapse a non-empty \'structure\' with a projection into a 'PComonad'.
 This is the most general semifold possible.

 @since 1.2.0
-}
psemifoldComonad ::
    forall
        (t :: (S -> Type) -> S -> Type)
        (f :: (S -> Type) -> S -> Type)
        (a :: S -> Type)
        (b :: S -> Type)
        (s :: S).
    ( PComonad f
    , PSemiTraversable t
    , forall (s' :: S). Semigroup (Term s' (f b))
    , PSubcategory f b
    , PSubcategory t a
    ) =>
    Term s ((a :--> f b) :--> t a :--> b)
psemifoldComonad = phoistAcyclic $ plam $ \f t -> pextract # (psemifoldMap # f # t)

{- | Collapse a possibly empty \'structure\' full of a 'Monoid'.

 @since 1.0.0
-}
pfold ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    ( PTraversable t
    , forall (s' :: S). Monoid (Term s' a)
    , PSubcategory t a
    ) =>
    Term s (t a :--> a)
pfold = phoistAcyclic $
    plam $ \t -> unTermCont $ do
        PConst res <- pmatchC (ptraverse_ # go # t)
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
    ( PTraversable t
    , forall (s' :: S). Monoid (Term s' b)
    , PSubcategory t a
    ) =>
    Term s ((a :--> b) :--> t a :--> b)
pfoldMap = phoistAcyclic $
    plam $ \f t -> unTermCont $ do
        PConst res <- pmatchC (ptraverse_ # (go # f) # t)
        pure res
  where
    go ::
        forall (s' :: S).
        Term s' ((a :--> b) :--> a :--> PConst b PUnit)
    go = phoistAcyclic $ plam $ \f x -> pcon . PConst $ f # x

{- | Collapse a possibly empty \'structure\' with a projection into a
 'PComonad'. This is the most general fold possible.

 @since 1.0.0
-}
pfoldComonad ::
    forall
        (t :: (S -> Type) -> S -> Type)
        (f :: (S -> Type) -> S -> Type)
        (a :: S -> Type)
        (b :: S -> Type)
        (s :: S).
    ( PComonad f
    , PTraversable t
    , forall (s' :: S). Monoid (Term s' (f b))
    , PSubcategory f b
    , PSubcategory t a
    ) =>
    Term s ((a :--> f b) :--> t a :--> b)
pfoldComonad = phoistAcyclic $ plam $ \f t -> pextract # (pfoldMap # f # t)

{- | Counts the number of elements in the \'structure\'.

 @since 1.0.0
-}
plength ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PTraversable t, PSubcategory t a) =>
    Term s (t a :--> PInteger)
plength = phoistAcyclic $ plam $ \t -> pfoldComonad # go # t
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
    ( PTraversable t
    , forall (s' :: S). Num (Term s' a)
    , PSubcategory t a
    ) =>
    Term s (t a :--> a)
psum = phoistAcyclic $ plam $ \t -> pfoldComonad # go # t
  where
    go ::
        forall (s' :: S).
        Term s' (a :--> PSum a)
    go = phoistAcyclic $ plam $ pcon . PSum
