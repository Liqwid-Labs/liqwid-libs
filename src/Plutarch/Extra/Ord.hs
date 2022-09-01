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

    -- *** Sortedness checking
    pisSortedBy,

    -- *** Uniqueness checking
    pallUnique,
    pallUniqueBy,
    ptryAllUnique,
    ptryAllUniqueBy,

    -- *** Sorted merging
    ptryMerge,
    ptryMergeBy,
    {-
    -- *** Sorting
    psort,
    psortBy,

    -- *** Nubbing
    pnubSort,
    pnubSortBy,
    -}
) where

import Data.Semigroup (Semigroup (stimes), stimesIdempotentMonoid)
import Plutarch.Extra.Maybe (ptraceIfNothing)
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

{- | A representation of a comparison at the Plutarch level. Equivalent to
 'Ordering' in Haskell.

 @since 3.6.0
-}
data POrdering (s :: S)
    = -- | Indicates a less-than relationship.
      --
      -- @since 3.6.0
      PLT
    | -- | Indicates equality.
      --
      -- @since 3.6.0
      PEQ
    | -- | Indicates a greater-than relationship.
      --
      -- @since 3.6.0
      PGT
    deriving stock
        ( -- | @since 3.6.0
          Show
        , -- | @since 3.6.0
          Generic
        )
    deriving anyclass
        ( -- | @since 3.6.0
          PShow
        , -- | @since 3.6.0
          PPartialOrd
        , -- | @since 3.6.0
          POrd
        )

-- | @since 3.6.0
instance PUnsafeLiftDecl POrdering where
    type PLifted POrdering = Ordering

-- | @since 3.6.0
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

-- | @since 3.6.0
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

-- | @since 3.6.0
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

-- | @since 3.6.0
instance Semigroup (Term s POrdering) where
    x <> y = pmatch x $ \case
        PLT -> pcon PLT
        PEQ -> y
        PGT -> pcon PGT
    stimes = stimesIdempotentMonoid

-- | @since 3.6.0
instance Monoid (Term s POrdering) where
    mempty = pcon PEQ

-- | @since 3.6.0
newtype PComparator (a :: S -> Type) (s :: S)
    = PComparator (Term s (a :--> a :--> POrdering))
    deriving stock
        ( -- | @since 3.6.0
          Generic
        )
    deriving anyclass
        ( -- | @since 3.6.0
          PlutusType
        )

-- | @since 3.6.0
instance DerivePlutusType (PComparator a) where
    type DPTStrat _ = PlutusTypeNewtype

{- | Given a type with a 'POrd' instance, construct a 'PComparator' from that
 instance.

 @since 3.6.0
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

 @since 3.6.0
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

 @since 3.6.0
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

 @since 3.6.0
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

 @since 3.6.0
-}
pmapComparator ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s ((b :--> a) :--> PComparator a :--> PComparator b)
pmapComparator = phoistAcyclic $
    plam $ \f cmp -> pcon . PComparator . plam $ \x y ->
        pcompareBy # cmp # (f # x) # (f # y)

{- | Reverses the ordering described by a 'PComparator'.

 @since 3.6.0
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

 @since 3.6.0
-}
pcompareBy ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> a :--> a :--> POrdering)
pcompareBy = phoistAcyclic $
    plam $ \cmp x y -> pto cmp # x # y

{- | Uses a 'PComparator' for an equality check.

 @since 3.6.0
-}
pequateBy ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> a :--> a :--> PBool)
pequateBy = phoistAcyclic $ plam $ \cmp x y -> pcon PEQ #== (pto cmp # x # y)

{- | Verifies that a list-like structure is ordered according to the
 'PComparator' argument.

 @since 3.6.0
-}
pisSortedBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> PBool)
pisSortedBy = phoistAcyclic $
    plam $ \cmp ->
        precList (\self x -> pelimList (go cmp self x) (pconstant True)) (const (pconstant True))
  where
    go ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' (ell a :--> PBool) ->
        Term s' a ->
        Term s' a ->
        Term s' (ell a) ->
        Term s' PBool
    go cmp self h peek t = pmatch (pcompareBy # cmp # h # peek) $ \case
        PGT -> pconstant False
        _ ->
            let result = self # t
             in pelimList
                    ( \peek2 _ -> pmatch (pcompareBy # cmp # peek # peek2) $ \case
                        PGT -> pconstant False
                        _ -> result
                    )
                    result
                    t

{- | Verifies that a list-like structure is both ordered (by the 'POrd' instance
 it's full of) and has no duplicates (by the 'PEq' instance it's full of).
 This can give any of the following results:

 * 'PNothing' if the structure is found to be unsorted;
 * 'PJust' 'PFalse' if the structure contains a duplicate; or
 * 'PJust' 'PTrue' if it is both sorted and contains no duplicates.

 @since 3.6.0
-}
pallUnique ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (POrd a, PElemConstraint ell a, PListLike ell) =>
    Term s (ell a :--> PMaybe PBool)
pallUnique = phoistAcyclic $ plam (pallUniqueBy # pfromOrd @a #)

{- | As 'pallUnique', but using a custom 'PComparator' to verify order and
 equality.

 @since 3.6.0
-}
pallUniqueBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> PMaybe PBool)
pallUniqueBy = phoistAcyclic $
    plam $ \cmp xs ->
        precList (\self x -> pelimList (go cmp self x) success) (const success) # xs
  where
    success :: forall (s' :: S). Term s' (PMaybe PBool)
    success = pcon . PJust . pconstant $ True
    pcompareByCont ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' a ->
        Term s' (PMaybe PBool) ->
        Term s' (PMaybe PBool)
    pcompareByCont cmp x y cont = pmatch (pcompareBy # cmp # x # y) $ \case
        PLT -> cont
        PEQ -> pcon . PJust . pconstant $ False
        PGT -> pcon PNothing
    go ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' (ell a :--> PMaybe PBool) ->
        Term s' a ->
        Term s' a ->
        Term s' (ell a) ->
        Term s' (PMaybe PBool)
    go cmp self x peek rest =
        pcompareByCont cmp x peek $
            let result = self # rest
             in pmatch result $ \case
                    PNothing -> result
                    PJust b ->
                        pif
                            b
                            (pelimList (\peek' _ -> pcompareByCont cmp peek peek' success) success rest)
                            result

{- | As 'pallUnique', but errors if the list-like argument is found to be
 unsorted instead of returning 'PNothing'.

 @since 3.6.0
-}
ptryAllUnique ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (POrd a, PElemConstraint ell a, PListLike ell) =>
    Term s (ell a :--> PBool)
ptryAllUnique = phoistAcyclic $ plam (ptryAllUniqueBy # pfromOrd @a #)

{- | As 'pallUniqueBy', but errors if the list-like argument is found to be
 unsorted instead of returning 'PNothing'.

 @since 3.6.0
-}
ptryAllUniqueBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> PBool)
ptryAllUniqueBy = phoistAcyclic $
    plam $ \cmp xs ->
        ptraceIfNothing "ptryAllUniqueBy: argument is unordered" $ pallUniqueBy # cmp # xs

{- | Merge two list-like structures, whose contents are sorted by the 'POrd'
 instance for their contents, into one sorted list-like structure. This will
 error if it finds that one of the list-like structures given to it as an
 argument is not sorted.

 @since 3.6.0
-}
ptryMerge ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (POrd a, PElemConstraint ell a, PListLike ell) =>
    Term s (ell a :--> ell a :--> ell a)
ptryMerge = phoistAcyclic $ plam (ptryMergeBy # pfromOrd @a #)

{- | As 'ptryMerge', but instead of using 'POrd' to determine sorting, uses a
 custom 'PComparator'. Will error out if one of the list-like structures given
 as an argument is not sorted according to the custom 'PComparator'.

 @since 3.6.0
-}
ptryMergeBy ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> ell a :--> ell a)
ptryMergeBy = phoistAcyclic $
    pfix #$ plam $ \self cmp xs ys ->
        -- Check if either side of the merge is exhausted; if it is, just check the
        -- remaining argument and yield.
        phandleList xs (passertSorted # cmp # ys) $ \xsHead xsTail ->
            phandleList ys (passertSorted # cmp # xs) $ \ysHead ysTail ->
                -- 'Peek ahead' of both tails to verify ordering holds.
                phandleList xsTail (go cmp xsHead ysHead ysTail) $ \xsPeek _ ->
                    phandleList ysTail (go cmp ysHead xsHead xsTail) $ \ysPeek _ ->
                        pmatch (pcompareBy # cmp # xsHead # xsPeek) $ \case
                            -- We are out-of-order, vomit.
                            PGT -> ptraceError "ptryMergeBy: argument list-like out of order"
                            _ -> pmatch (pcompareBy # cmp # ysHead # ysPeek) $ \case
                                -- We are out-of-order, vomit.
                                PGT -> ptraceError "ptryMergeBy: argument list-like out of order"
                                -- Perform one step of the merge.
                                _ -> pmatch (pcompareBy # cmp # xsHead # ysHead) $ \case
                                    -- ysHead goes first.
                                    PGT -> pcons # ysHead #$ self # cmp # xs # ysTail
                                    -- xsHead goes first.
                                    _ -> pcons # xsHead #$ self # cmp # xsTail # ys
  where
    go ::
        forall (s' :: S).
        Term s' (PComparator a) ->
        Term s' a ->
        Term s' a ->
        Term s' (ell a) ->
        Term s' (ell a)
    go cmp single h t = pmatch (pcompareBy # cmp # single # h) $ \case
        PGT -> pcons # h #$ pcons # single #$ passertSorted # cmp # t
        _ -> pcons # single #$ pcons # h #$ passertSorted # cmp # t

-- Helpers

-- pelimList with the list-like first, and handles the 'nil case' before the
-- 'cons' case
phandleList ::
    forall (a :: S -> Type) (r :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (ell a) ->
    Term s r ->
    (Term s a -> Term s (ell a) -> Term s r) ->
    Term s r
phandleList xs whenNil whenCons = pelimList whenCons whenNil xs

-- ensures the argument is sorted by the comparator, erroring if not
passertSorted ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PComparator a :--> ell a :--> ell a)
passertSorted = phoistAcyclic $
    plam $ \cmp xs ->
        pif
            (pisSortedBy # cmp # xs)
            xs
            (ptraceError "ptryMergeBy: argument list-like out of order")
