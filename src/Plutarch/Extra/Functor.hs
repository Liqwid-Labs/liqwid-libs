{-# LANGUAGE TypeApplications #-}
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
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V1.Maybe (PMaybeData (PDJust, PDNothing))
import Plutarch.Builtin (
    PAsData,
    PBuiltinList,
    PBuiltinPair,
    PIsData,
    pdata,
    pfromData,
    pfstBuiltin,
    ppairDataBuiltin,
    psndBuiltin,
 )
import Plutarch.DataRepr (pdcons, pdnil, pfield)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Extra.Function (pconst, pidentity)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Lift (PUnsafeLiftDecl)
import Plutarch.List (PList, pmap)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unit (PUnit (PUnit))

-- | @since 1.0.0
class PFunctor (f :: (S -> Type) -> S -> Type) where
    type PSubcategory f :: (S -> Type) -> Constraint
    pfmap ::
        forall (a :: S -> Type) (b :: S -> Type) (s :: S).
        (PSubcategory f a, PSubcategory f b) =>
        Term s ((a :--> b) :--> f a :--> f b)

-- | @since 1.0.0
instance PFunctor PMaybe where
    type PSubcategory PMaybe = Top
    pfmap = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            pure . pcon $ case t' of
                PNothing -> PNothing
                PJust t'' -> PJust $ f # t''

-- | @since 1.0.0
instance PFunctor PMaybeData where
    type PSubcategory PMaybeData = PIsData
    pfmap = phoistAcyclic $
        plam $ \f t -> unTermCont $ do
            t' <- pmatchC t
            case t' of
                PDNothing _ -> pure . pcon . PDNothing $ pdnil
                PDJust t'' -> do
                    x <- pletC (pfromData $ pfield @"_0" # t'')
                    res <- pletC (f # x)
                    pure . pcon . PDJust $ pdcons # pdata res # pdnil

-- | @since 1.0.0
instance PFunctor PList where
    type PSubcategory PList = Top
    pfmap = phoistAcyclic $ plam $ \f t -> pmap # f # t

-- | @since 1.0.0
instance PFunctor PBuiltinList where
    type PSubcategory PBuiltinList = PUnsafeLiftDecl
    pfmap = phoistAcyclic $ plam $ \f t -> pmap # f # t

-- | @since 1.0.0
instance (PIsData k) => PFunctor (PMap k) where
    type PSubcategory (PMap k) = PIsData
    pfmap = psecond

-- | @since 1.0.0
instance PFunctor (PPair a) where
    type PSubcategory (PPair a) = Top
    pfmap = psecond

-- | @since 1.0.0
instance PFunctor (PEither e) where
    type PSubcategory (PEither e) = Top
    pfmap = psecond

-- | @since 1.0.0
(#<$) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s a ->
    Term s (f b) ->
    Term s (f a)
x #<$ f = pfmap # (pconst # x) # f

-- | @since 1.0.0
(#$>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s (f a) ->
    Term s b ->
    Term s (f b)
(#$>) = flip (#<$)

-- | @since 1.0.0
(#<$>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s (a :--> b) ->
    Term s (f a) ->
    Term s (f b)
f #<$> t = pfmap # f # t

-- | @since 1.0.0
(#<&>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s (f a) ->
    Term s (a :--> b) ->
    Term s (f b)
(#<&>) = flip (#<$>)

-- | @since 1.0.0
pvoid ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f PUnit) =>
    Term s (f a) ->
    Term s (f PUnit)
pvoid t = t #$> pcon PUnit

-- | @since 1.0.0
class PBifunctor (f :: (S -> Type) -> (S -> Type) -> S -> Type) where
    type PSubcategoryLeft f :: (S -> Type) -> Constraint
    type PSubcategoryRight f :: (S -> Type) -> Constraint
    {-# MINIMAL pbimap | pfirst, psecond #-}
    pbimap ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type) (s :: S).
        ( PSubcategoryLeft f a
        , PSubcategoryLeft f c
        , PSubcategoryRight f b
        , PSubcategoryRight f d
        ) =>
        Term s ((a :--> c) :--> (b :--> d) :--> f a b :--> f c d)
    pbimap = phoistAcyclic $ plam $ \f g t -> pfirst # f # (psecond # g # t)
    pfirst ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
        ( PSubcategoryLeft f a
        , PSubcategoryLeft f c
        , PSubcategoryRight f b
        ) =>
        Term s ((a :--> c) :--> f a b :--> f c b)
    pfirst = phoistAcyclic $ plam $ \f t -> pbimap # f # pidentity # t
    psecond ::
        forall (a :: S -> Type) (b :: S -> Type) (d :: S -> Type) (s :: S).
        ( PSubcategoryLeft f a
        , PSubcategoryRight f b
        , PSubcategoryRight f d
        ) =>
        Term s ((b :--> d) :--> f a b :--> f a d)
    psecond = phoistAcyclic $ plam $ \g t -> pbimap # pidentity # g # t

-- | @since 1.0.0
instance PBifunctor PPair where
    type PSubcategoryLeft PPair = Top
    type PSubcategoryRight PPair = Top
    pbimap = phoistAcyclic $
        plam $ \f g t -> unTermCont $ do
            PPair x y <- pmatchC t
            pure . pcon . PPair (f # x) $ g # y

-- | @since 1.0.0
instance PBifunctor PEither where
    type PSubcategoryLeft PEither = Top
    type PSubcategoryRight PEither = Top
    pbimap = phoistAcyclic $
        plam $ \f g t -> unTermCont $ do
            t' <- pmatchC t
            pure . pcon $ case t' of
                PLeft x -> PLeft $ f # x
                PRight y -> PRight $ g # y

-- | @since 1.0.0
instance PBifunctor PMap where
    type PSubcategoryLeft PMap = PIsData
    type PSubcategoryRight PMap = PIsData
    pbimap ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type) (s :: S).
        (PIsData a, PIsData b, PIsData c, PIsData d) =>
        Term s ((a :--> b) :--> (c :--> d) :--> PMap a c :--> PMap b d)
    pbimap = phoistAcyclic $
        plam $ \f g t -> unTermCont $ do
            PMap t' <- pmatchC t
            pure . pcon . PMap $ pfmap # (go # f # g) # t'
      where
        go ::
            forall (s' :: S).
            Term
                s'
                ( (a :--> b)
                    :--> (c :--> d)
                    :--> PBuiltinPair (PAsData a) (PAsData c)
                    :--> PBuiltinPair (PAsData b) (PAsData d)
                )
        go = phoistAcyclic $
            plam $ \f g p -> unTermCont $ do
                k <- pletC (pfromData $ pfstBuiltin # p)
                v <- pletC (pfromData $ psndBuiltin # p)
                k' <- pletC (f # k)
                v' <- pletC (g # v)
                pure $ ppairDataBuiltin # pdata k' # pdata v'
