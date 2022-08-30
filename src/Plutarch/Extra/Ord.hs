{-# LANGUAGE TypeApplications #-}
-- Needed to 'link' Ordering and POrdering
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
 Module: Plutarch.Extra.Ord
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Ordering-related helpers and functionality.
-}
module Plutarch.Extra.Ord (
    -- * Types
    POrdering (..),
    PComparator,

    -- * Functions

    -- ** Creating comparators
    pfromOrd,
    pfromOrdBy,

    -- ** Combining comparators
    pproductComparator,
    psumComparator,

    -- ** Transforming comparators
    pmapComparator,
    preverseComparator,

    -- ** Using comparators

    -- *** Basic
    pcompareBy,
    pequateBy,

    -- *** Sorting
    psort,
    psortBy,

    -- *** Merging
    ptryMerge,
    ptryMergeBy,

    -- *** Uniqueness checking
    pAllUnique,
    pAllUniqueBy,
) where

import Data.Semigroup (Semigroup (stimes), stimesIdempotentMonoid)
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
import Plutarch.Lift (
    PConstantDecl (
        PConstantRepr,
        PConstanted,
        pconstantFromRepr,
        pconstantToRepr
    ),
    PUnsafeLiftDecl (PLifted),
 )
import qualified Plutarch.List as PList

{- | Sorts a list-like structure full of a 'POrd' instance.

 This uses [merge sort](https://en.wikipedia.org/wiki/Merge_sort), which is
 also [stable](https://en.wikipedia.org/wiki/Sorting_algorithm#Stability).
 This means that it requires a linearithmic ($n \log(n)$) number of
 comparisons, as with all comparison sorts.

 @since 3.4.0
-}
psort ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (POrd a, PElemConstraint ell a, PElemConstraint ell (ell a), PListLike ell) =>
    Term s (ell a :--> ell a)
psort = phoistAcyclic $ plam $ \xs -> psortBy # pfromOrd @a # xs

{- | As 'psort', but using a custom 'PComparator'.

 @since 3.4.0
-}
psortBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PElemConstraint ell (ell a), PListLike ell) =>
    Term s (PComparator a :--> ell a :--> ell a)
psortBy = phoistAcyclic $
    plam $ \cmp xs ->
        pmergeAll # cmp #$ pmergeStart_2_3 # cmp # xs

{- | Given two list-like structures which are already sorted by their 'POrd'
 instances, attempt to merge them, preserving their order. If one of the
 structures is found not to be ordered, error out instead.

 @since 3.4.0
-}
ptryMerge ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell, POrd a) =>
    Term s (ell a :--> ell a :--> ell a)
ptryMerge = phoistAcyclic $ plam $ \xs -> ptryMergeBy # pfromOrd @a # xs

{- | As 'pmerge', but specifying the ordering by a custom 'PComparator'.

 @since 3.4.0
-}
ptryMergeBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> ell a :--> ell a)
ptryMergeBy = phoistAcyclic $
    pfix #$ plam $ \self cmp xs ys ->
        pmatch (PList.puncons # xs) $ \case
            -- Exhausted left, check ordering and go if it works
            PNothing -> passertOrderBy # cmp # ys
            PJust xs' -> pmatch (PList.puncons # ys) $ \case
                -- Exhausted right, check ordering and go if it works
                PNothing -> passertOrderBy # cmp # xs
                PJust ys' -> pmatch xs' $ \(PPair xsHead xsTail) ->
                    pmatch ys' $ \(PPair ysHead ysTail) ->
                        pmatch (PList.puncons # xsTail) $ \case
                            -- Singletons are always ordered
                            PNothing -> pmatch (pcompareBy # cmp # xsHead # ysHead) $ \case
                                PGT -> pcons # ysHead #$ self # cmp # xs # ysTail
                                _ -> pcons # xsHead #$ passertOrderBy # cmp # ys
                            PJust xs'' -> pmatch (PList.puncons # ysTail) $ \case
                                -- Singletons are always ordered
                                PNothing -> pmatch (pcompareBy # cmp # xsHead # ysHead) $ \case
                                    PGT -> pcons # ysHead #$ passertOrderBy # cmp # xs
                                    _ -> pcons # xsHead #$ self # cmp # xsTail # ys
                                -- Need to look further to ensure we're not mis-ordered
                                PJust ys'' -> pmatch xs'' $ \(PPair xsPeek _) ->
                                    -- Verify by look-ahead that xsHead and xsPeek are ordered
                                    pmatch (pcompareBy # cmp # xsHead # xsPeek) $ \case
                                        -- We are out of order, vomit.
                                        PGT -> ptraceError "ptryMergeBy: argument list-like out of order"
                                        _ -> pmatch ys'' $ \(PPair ysPeek _) ->
                                            -- Verify by look-ahead that ysHead and ysPeek are ordered
                                            pmatch (pcompareBy # cmp # ysHead # ysPeek) $ \case
                                                -- We are out of order, vomit.
                                                PGT -> ptraceError "ptryMergeBy: argument list-like out of order"
                                                -- Actually perform the merge.
                                                _ -> pmatch (pcompareBy # cmp # xsHead # ysHead) $ \case
                                                    PGT -> pcons # ysHead #$ self # cmp # xs # ysTail
                                                    _ -> pcons # xsHead #$ self # cmp # xsTail # ys

{- | Verifies if every element of the input list-like is unique according to its
 'PEq' instance. Relies on a 'POrd' instance for speed, but only works if the
 input is sorted according to that instance.

 Returns @'PJust' 'PTrue'@ if every element is unique as specified above,
 @'PJust' 'PFalse'@ if a duplicate exists, and 'PNothing' if the input isn't
 ordered.

 @since 3.4.0
-}
pAllUnique ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell, POrd a) =>
    Term s (ell a :--> PMaybe PBool)
pAllUnique = phoistAcyclic $ plam $ \xs -> pAllUniqueBy # pfromOrd @a # xs

{- | As 'pAllUnique', but relies on a custom 'PComparator' for both equality and
 ordering (instead of a 'PEq' and 'POrd' instance respectively).

 @since 3.4.0
-}
pAllUniqueBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> PMaybe PBool)
pAllUniqueBy = phoistAcyclic $
    plam $ \cmp ->
        precList (go cmp) (const (pcon . PJust . pconstant $ True))
  where
    go ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' (ell a :--> PMaybe PBool) ->
        Term s' a ->
        Term s' (ell a) ->
        Term s' (PMaybe PBool)
    go cmp self x xs = pmatch (PList.puncons # xs) $ \case
        -- Singletons are always unique no matter their ordering.
        PNothing -> pcon . PJust . pconstant $ True
        PJust xs' -> pmatch xs' $ \(PPair h _) ->
            pmatch (pcompareBy # cmp # x # h) $ \case
                -- Order holds, so defer to the outcome of self.
                PLT -> self # xs
                -- Duplicate.
                PEQ -> pmatch (self # xs) $ \case
                    -- If we're out-of-order, duplicates aren't relevant.
                    PNothing -> pcon PNothing
                    PJust _ -> pcon . PJust . pconstant $ False
                -- Out of order.
                PGT -> pcon PNothing

{- | A representation of a comparison at the Plutarch level. Equivalent to
 'Ordering' in Haskell.

 @since 3.4.0
-}
data POrdering (s :: S)
    = -- | Indicates a less-than relationship.
      --
      -- @since 3.4.0
      PLT
    | -- | Indicates equality.
      --
      -- @since 3.4.0
      PEQ
    | -- | Indicates a greater-than relationship.
      --
      -- @since 3.4.0
      PGT
    deriving stock
        ( -- | @since 3.4.0
          Show
        )

-- | @since 3.4.0
instance PUnsafeLiftDecl POrdering where
    type PLifted POrdering = Ordering

-- | @since 3.4.0
instance PConstantDecl Ordering where
    type PConstantRepr Ordering = Integer
    type PConstanted Ordering = POrdering
    pconstantToRepr = \case
        LT -> 0
        EQ -> 1
        GT -> 2
    pconstantFromRepr = \case
        0 -> pure LT
        1 -> pure EQ
        2 -> pure GT
        _ -> Nothing

-- | @since 3.4.0
instance PlutusType POrdering where
    type PInner POrdering = PInteger
    pcon' = \case
        PLT -> 0
        PEQ -> 1
        PGT -> 2
    pmatch' x f =
        pif
            (x #== 0)
            (f PLT)
            ( pif
                (x #== 1)
                (f PEQ)
                (f PGT)
            )

-- | @since 3.4.0
instance PEq POrdering where
    x #== y = pmatch x $ \case
        PLT -> pmatch y $ \case
            PLT -> pcon PTrue
            _ -> pcon PFalse
        PEQ -> pmatch y $ \case
            PEQ -> pcon PTrue
            _ -> pcon PFalse
        PGT -> pmatch y $ \case
            PGT -> pcon PTrue
            _ -> pcon PFalse

-- | @since 3.4.0
instance Semigroup (Term s POrdering) where
    x <> y = pmatch x $ \case
        PLT -> pcon PLT
        PEQ -> y
        PGT -> pcon PGT
    stimes = stimesIdempotentMonoid

-- | @since 3.4.0
instance Monoid (Term s POrdering) where
    mempty = pcon PEQ

-- TODO: PShow, PPartialOrd, POrd

-- | @since 3.4.0
newtype PComparator (a :: S -> Type) (s :: S)
    = PComparator (Term s (a :--> a :--> POrdering))
    deriving stock
        ( -- | @since 3.4.0
          Generic
        )
    deriving anyclass
        ( -- | @since 3.4.0
          PlutusType
        )

-- | @since 3.4.0
instance DerivePlutusType (PComparator a) where
    type DPTStrat _ = PlutusTypeNewtype

{- | Given a type with a 'POrd' instance, construct a 'PComparator' from that
 instance.

 @since 3.4.0
-}
pfromOrd ::
    forall (a :: S -> Type) (s :: S).
    (POrd a) =>
    Term s (PComparator a)
pfromOrd = pcon . PComparator $
    phoistAcyclic $
        plam $ \x y ->
            pif (x #== y) (pcon PEQ) (pif (x #< y) (pcon PLT) (pcon PGT))

{- | As 'pfromOrd', but instead uses a projection function into the 'POrd'
 instance to construct the 'PComparator'. Allows other \'-by\' behaviours.

 @since 3.4.0
-}
pfromOrdBy ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    (POrd a) =>
    Term s ((b :--> a) :--> PComparator b)
pfromOrdBy = phoistAcyclic $
    plam $ \f -> pcon . PComparator . plam $ \x y ->
        unTermCont $ do
            fx <- pletC (f # x)
            fy <- pletC (f # y)
            pure $
                pif
                    (fx #== fy)
                    (pcon PEQ)
                    ( pif
                        (fx #< fy)
                        (pcon PLT)
                        (pcon PGT)
                    )

{- | Given a way of \'separating\' a @c@ into an @a@ and a @b@, as well as
 'PComparator's for @a@ and @b@, make a 'PComparator' for @c@.

 = Note

 This uses the fact that 'POrdering' is a 'Semigroup', and assumes that @c@ is
 a tuple of @a@ and @b@ in some sense, and that it should be ordered
 lexicographically on that basis.

 @since 3.4.0
-}
pproductComparator ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    Term s ((c :--> PPair a b) :--> PComparator a :--> PComparator b :--> PComparator c)
pproductComparator = phoistAcyclic $
    plam $ \split cmpA cmpB -> pcon . PComparator . plam $ \x y ->
        pmatch (split # x) $ \(PPair xA xB) ->
            pmatch (split # y) $ \(PPair yA yB) ->
                (pcompareBy # cmpA # xA # yA) <> (pcompareBy # cmpB # xB # yB)

{- | Given a way of \'discriminating\' a @c@ into either an @a@ or a @b@, as
 well as 'PComparator's for @a@ and @b@, make a 'PComparator' for @c@.

 = Note

 This assumes that \'@c@s that are @a@s\' should be ordered before \'@c@s that
 are @b@s\'.

 @since 3.4.0
-}
psumComparator ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    Term s ((c :--> PEither a b) :--> PComparator a :--> PComparator b :--> PComparator c)
psumComparator = phoistAcyclic $
    plam $ \discriminate cmpA cmpB -> pcon . PComparator . plam $ \x y ->
        pmatch (discriminate # x) $ \case
            PLeft xA -> pmatch (discriminate # y) $ \case
                PLeft yA -> pcompareBy # cmpA # xA # yA
                PRight _ -> pcon PLT
            PRight xB -> pmatch (discriminate # y) $ \case
                PRight yB -> pcompareBy # cmpB # xB # yB
                PLeft _ -> pcon PGT

{- | Given a projection from a type to another type which we have a
 'PComparator' for, construct a new 'PComparator'.

 @since 3.4.0
-}
pmapComparator ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s ((b :--> a) :--> PComparator a :--> PComparator b)
pmapComparator = phoistAcyclic $
    plam $ \f cmp -> pcon . PComparator . plam $ \x y ->
        pcompareBy # cmp # (f # x) # (f # y)

{- | Reverses the ordering described by a 'PComparator'.

 @since 3.4.0
-}
preverseComparator ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> PComparator a)
preverseComparator = phoistAcyclic $
    plam $ \cmp -> pcon . PComparator . plam $ \x y ->
        pmatch (pcompareBy # cmp # x # y) $ \case
            PEQ -> pcon PEQ
            PLT -> pcon PGT
            PGT -> pcon PLT

{- | \'Runs\' a 'PComparator'.

 @since 3.4.0
-}
pcompareBy ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> a :--> a :--> POrdering)
pcompareBy = phoistAcyclic $
    plam $ \cmp x y -> pto cmp # x # y

{- | Uses a 'PComparator' for an equality check.

 @since 3.4.0
-}
pequateBy ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> a :--> a :--> PBool)
pequateBy = phoistAcyclic $ plam $ \cmp x y -> pcon PEQ #== (pto cmp # x # y)

-- Helpers

pmergeAll ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PElemConstraint ell (ell a), PListLike ell) =>
    Term s (PComparator a :--> ell (ell a) :--> ell a)
pmergeAll = phoistAcyclic $
    pfix #$ plam $ \self cmp xss ->
        pmatch (PList.puncons # xss) $ \case
            PNothing -> pnil
            PJust xss' -> pmatch xss' $ \(PPair h t) ->
                pmatch (PList.puncons # t) $ \case
                    PNothing -> h
                    PJust _ -> self # cmp #$ go # cmp # t
  where
    go ::
        forall (s' :: S).
        Term s' (PComparator a :--> ell (ell a) :--> ell (ell a))
    go = phoistAcyclic $
        pfix #$ plam $ \self cmp xss ->
            pmatch (PList.puncons # xss) $ \case
                PNothing -> pnil
                PJust xss' -> pmatch xss' $ \(PPair h t) ->
                    pmatch (PList.puncons # t) $ \case
                        PNothing -> xss
                        PJust xss'' -> pmatch xss'' $ \(PPair h' t') ->
                            pcons # (pmergeUnsafe # cmp # h # h') # (self # cmp # t')

pmergeUnsafe ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> ell a :--> ell a)
pmergeUnsafe = phoistAcyclic $
    pfix #$ plam $ \self cmp xs ys ->
        pmatch (PList.puncons # xs) $ \case
            -- Exhausted xs, yield ys as-is.
            PNothing -> ys
            PJust xs' -> pmatch (PList.puncons # ys) $ \case
                -- Exhausted ys, yield xs as-is.
                PNothing -> xs
                PJust ys' -> pmatch xs' $ \(PPair leftH leftT) ->
                    pmatch ys' $ \(PPair rightH rightT) ->
                        pmatch (pcompareBy # cmp # leftH # rightH) $ \case
                            -- Right before left.
                            PGT -> pcons # rightH #$ pcons # leftH # (self # cmp # leftT # rightT)
                            -- Left before right.
                            _ -> pcons # leftH #$ pcons # rightH # (self # cmp # leftT # rightT)

pmergeStart_2_3 ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell, PElemConstraint ell (ell a)) =>
    Term s (PComparator a :--> ell a :--> ell (ell a))
pmergeStart_2_3 = phoistAcyclic $
    pfix #$ plam $ \self cmp ->
        pmatchList pnil $ \_0 ->
            pmatchList (plist [psingletonUnhoisted _0]) $ \_1 ->
                pmatchList (plist [psort2 cmp _0 _1]) $ \_2 ->
                    pmatchList (plist [psort3 cmp _0 _1 _2]) $ \_3 rest ->
                        pcons # psort4 cmp _0 _1 _2 _3 #$ self # cmp # rest
  where
    psort2 ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' a ->
        Term s' (ell a)
    psort2 cmp _0 _1 = pswap cmp _0 _1 $
        \_0 _1 -> plist [_0, _1]
    psort3 ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' a ->
        Term s' a ->
        Term s' (ell a)
    psort3 cmp _0 _1 _2 = pswap cmp _0 _2 $
        \_0 _2 -> pswap cmp _0 _1 $
            \_0 _1 -> pswap cmp _1 _2 $
                \_1 _2 -> plist [_0, _1, _2]
    psort4 ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' a ->
        Term s' a ->
        Term s' a ->
        Term s' (ell a)
    psort4 cmp _0 _1 _2 _3 = pswap cmp _0 _2 $
        \_0 _2 -> pswap cmp _1 _3 $
            \_1 _3 -> pswap cmp _0 _1 $
                \_0 _1 -> pswap cmp _2 _3 $
                    \_2 _3 -> pswap cmp _1 _2 $
                        \_1 _2 -> plist [_0, _1, _2, _3]
    pswap ::
        forall (r :: S -> Type) (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' a ->
        (Term s' a -> Term s' a -> Term s' r) ->
        Term s' r
    pswap cmp x y cont = pmatch (pcompareBy # cmp # x # y) $ \case
        PGT -> cont y x
        _ -> cont x y

-- Unhoisted psingleton
psingletonUnhoisted ::
    forall (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s a ->
    Term s (ell a)
psingletonUnhoisted x = plist [x]

-- Turns any Foldable into a list-like, unhoisted
plist ::
    forall (ell :: (S -> Type) -> S -> Type) (f :: Type -> Type) (a :: S -> Type) (s :: S).
    (Foldable f, PListLike ell, PElemConstraint ell a) =>
    f (Term s a) ->
    Term s (ell a)
plist = foldr (\x xs -> pcons # x # xs) pnil

-- CPS-chaining-friendly uncons.
pmatchList ::
    forall (r :: S -> Type) (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    -- Continuation for \'nil\'.
    Term s r ->
    -- Continuation for \'cons\'.
    (Term s a -> Term s (ell a) -> Term s r) ->
    -- The list-like to match on.
    Term s (ell a) ->
    Term s r
pmatchList = flip pelimList

passertOrderBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> ell a)
passertOrderBy = phoistAcyclic $ plam $ \cmp -> pelimList (go cmp) pnil
  where
    go ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' (ell a) ->
        Term s' (ell a)
    go cmp x xs = pmatch (PList.puncons # xs) $ \case
        PNothing -> pcons # x # pnil
        PJust xs' -> pmatch xs' $ \(PPair h _) ->
            pmatch (pcompareBy # cmp # x # h) $ \case
                -- We are out of order, vomit.
                PGT -> ptraceError "ptryMergeBy: argument list-like out of order"
                _ -> pcons # x # xs
