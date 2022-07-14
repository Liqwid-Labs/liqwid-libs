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
    PType,
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Api.V1.AssocMap (KeyGuarantees, PMap (PMap))
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
import Plutarch.Extra.Boring (PBoring (pboring))
import Plutarch.Extra.Function (pconst, pidentity)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Lift (PUnsafeLiftDecl)
import Plutarch.List (PList, pmap)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))

{- | Describes a Plutarch-level covariant endofunctor. However, unlike in
 Haskell, the endofunctor is defined over a subcategory of @Plut@, rather than
 all of it.

 Put another way, this is the Plutarch equivalent to 'Functor', but unlike in
 Haskell, instead of requiring full parametricity, we are allowed to constrain
 what we are parametric over.

 = Laws

 Formally, @f@ must be an endofunctor on a subcategory of @Plut@, as described
 by the 'PSubcategory' constraint. This means that the following must hold:

 * @'pfmap' '#' 'Plutarch.Extra.Category.pidentity'@ @=@
 @'Plutarch.Extra.Category.pidentity'@
 * @'pfmap' '#' (f 'Plutarch.Extra.Category.#>>>' g)@ @=@ @('pfmap' '#' f)
 'Plutarch.Extra.Category.#>>>' ('pfmap' '#' g)@

 If @'PSubcategory' f@ is 'Top' (that is, @f@ is defined as an endofunctor on
 /all/ of @Plut@), the second law is a free theorem; however, in any other
 case, it may not be.

 @since 1.0.0
-}
class PFunctor (f :: (S -> Type) -> S -> Type) where
    {-# MINIMAL pfmap #-}

    -- | Describes the subcategory of @Plut@ that @f@ is an endofunctor on. Put
    -- another way, this describes what kind of types @f@ is \'parametric
    -- over\'.
    --
    -- Common choices for this are:
    --
    -- * 'Top', which means \'parametric over anything of kind @'S' -> 'Type'@\'
    -- * 'PIsData', which means \'parametric over things which are
    -- @Data@-encodable\'
    -- * 'PUnsafeLiftDecl', which means \'parametric over things that have a
    -- Haskell-level equivalent\'
    type PSubcategory f :: (S -> Type) -> Constraint

    pfmap ::
        forall (a :: S -> Type) (b :: S -> Type) (s :: S).
        (PSubcategory f a, PSubcategory f b) =>
        Term s ((a :--> b) :--> f a :--> f b)

    -- | Replace all values to be computed with a fixed value. Defaults to
    -- @'pfmap' 'pconst'@, but could be more efficient for some 'PFunctor's.
    --
    -- @since 1.2.0
    {-# INLINEABLE pfconst #-}
    pfconst ::
        forall (a :: S -> Type) (b :: S -> Type) (s :: S).
        (PSubcategory f a, PSubcategory f b) =>
        Term s (a :--> f b :--> f a)
    pfconst = phoistAcyclic $ plam $ \x ys -> pfmap # (pconst # x) # ys

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
instance forall (s :: KeyGuarantees) (k :: PType). (PIsData k) => PFunctor (PMap s k) where
    type PSubcategory (PMap s k) = PIsData
    pfmap = psecond

-- | @since 1.0.0
instance PFunctor (PPair a) where
    type PSubcategory (PPair a) = Top
    pfmap = psecond

-- | @since 1.0.0
instance PFunctor (PEither e) where
    type PSubcategory (PEither e) = Top
    pfmap = psecond

{- | Infix, 'Term'-lifted version of 'pfconst'.

 @since 1.0.0
-}
(#<$) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s a ->
    Term s (f b) ->
    Term s (f a)
x #<$ f = pfconst # x # f

infixl 4 #<$

{- | Flipped version of '#<$'.

 @since 1.0.0
-}
(#$>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s (f a) ->
    Term s b ->
    Term s (f b)
(#$>) = flip (#<$)

{- | Infix, 'Term'-level version of 'pfmap'.

 @since 1.0.0
-}
(#<$>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s (a :--> b) ->
    Term s (f a) ->
    Term s (f b)
f #<$> t = pfmap # f # t

infixl 4 #<$>

{- | Flipped version of '#<$>'.

 @since 1.0.0
-}
(#<&>) ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b) =>
    Term s (f a) ->
    Term s (a :--> b) ->
    Term s (f b)
(#<&>) = flip (#<$>)

infixl 1 #<&>

{- | Erases every location in the input.

 @since 1.2.0
-}
pvoid ::
    forall (f :: (S -> Type) -> S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PFunctor f, PSubcategory f a, PSubcategory f b, PBoring b) =>
    Term s (f a) ->
    Term s (f b)
pvoid t = pfconst # pboring # t

{- | Similar to 'PFunctor', but is covariant in /two/ parameters instead of one.
 This means that types like 'PEither' don't need to be partially applied, as
 is the case with 'PFunctor'. Formally, this represents a Plutarch-level
 covariant bifunctor; just as with 'PFunctor' however, it is defined over a
 subcategory of 'Plut'.

 Similarly to 'PFunctor', this is the Plutarch equivalent of 'Bifunctor'.

 = Laws

 Formally, @f@ must be a bifunctor on a subcategory of @Plut@, as described by
 'PSubcategoryLeft' (for the first parameter) and 'PSubcategoryRight' (for the
 second). For 'pbimap', this means the following must hold:

 * @'pbimap' '#' 'Plutarch.Extra.Category.pidentity' '#'
 'Plutarch.Extra.Category.pidentity'@ @=@
 @'Plutarch.Extra.Category.pidentity'@
 * @'pbimap' '#' (f1 'Plutarch.Extra.Category.#>>>' f2) '#' (g1
 'Plutarch.Extra.Category.#>>>' g2)@ @=@ @('pbimap' '#' f1 '#' g1)
 'Plutarch.Extra.Category.#>>>' ('pbimap' '#' f2 '#' g2)@

 Furthermore, @'PSubcategoryLeft f' ~ 'PSubcategoryRight' f@ should hold; this
 may be required in the future. If both @'PSubcategoryLeft' f@ and
 @'PSubcategoryRight' f@ are 'Top', the second law is a free theorem; however,
 this does not hold in general.

 If you define 'pfirst' and 'psecond', the following must also hold:

 * @'pfirst' '#' 'Plutarch.Extra.Category.pidentity'@ @=@ @'psecond' '#'
 'Plutarch.Extra.Category.pidentity'@ @=@
 @'Plutarch.Extra.Category.pidentity'@
 * @'pfirst' '#' f@ @=@ @'pbimap' '#' f '#'
 'Plutarch.Extra.Category.pidentity'@
 * @'psecond' '#' f@ @=@ @'pbimap' '#' 'Plutarch.Extra.Category.pidentity' '#'
 f@
 * @'pfirst' '#' (f 'Plutarch.Extra.Category.#>>>' g)@ @=@ @('pfirst' '#' f)
 'Plutarch.Extra.Category.#>>>' ('pfirst' '#' g)@
 * @'psecond' '#' (f 'Plutarch.Extra.Category.#>>>' g)@ @=@ @('psecond' '#' f)
 'Plutarch.Extra.Category.#>>>' ('psecond' '#' g)@

 If you define 'pfirst' and 'psecond' /instead/ of 'pbimap', the following
 must also hold:

 * @('pfirst' '#' f) 'Plutarch.Extra.Category.#>>>' ('psecond' '#' g)@ @=@
 @('psecond' '#' g) 'Plutarch.Extra.Category.#>>>' ('pfirst' '#' f)@ @=@
 @'pbimap' '#' f '#' g@

 = Note

 If @f a@ is also an instance of 'PFunctor', @'PSubcategoryRight' f ~
 'PSubcategory' (f a)@ should hold, and we should have @'pfmap' = 'psecond'@;
 once again, this is not currently enforced, but may be in the future.

 @since 1.0.0
-}
class PBifunctor (f :: (S -> Type) -> (S -> Type) -> S -> Type) where
    -- | Similar to 'PSubcategory', but for only the first parameter of @f@. See
    -- the documentation on 'PSubcategory' for common choices here.
    type PSubcategoryLeft f :: (S -> Type) -> Constraint

    -- | Similar to 'PSubcategory', but for only the second parameter of @f@.
    -- See the documentation on 'PSubcategory' for common choices here.
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
instance forall (keys :: KeyGuarantees). PBifunctor (PMap keys) where
    type PSubcategoryLeft (PMap keys) = PIsData
    type PSubcategoryRight (PMap keys) = PIsData
    pbimap ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type) (s :: S).
        (PIsData a, PIsData b, PIsData c, PIsData d) =>
        Term s ((a :--> b) :--> (c :--> d) :--> PMap keys a c :--> PMap keys b d)
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
