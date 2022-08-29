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
    pcompareBy,
    pequateBy,
) where

import Data.Semigroup (Semigroup (stimes), stimesIdempotentMonoid)
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
    plam $ \f -> pcon . PComparator $
        plam $ \x y ->
            plet (f # x) $ \fx ->
                plet (f # y) $ \fy ->
                    pif (fx #== fy) (pcon PEQ) (pif (fx #< fy) (pcon PLT) (pcon PGT))

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
    plam $ \split cmpA cmpB ->
        pmatch cmpA $ \(PComparator fA) ->
            pmatch cmpB $ \(PComparator fB) ->
                pcon . PComparator . plam $ \x y ->
                    pmatch (split # x) $ \(PPair xA xB) ->
                        pmatch (split # y) $ \(PPair yA yB) ->
                            (fA # xA # yA) <> (fB # xB # yB)

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
    plam $ \discriminate cmpA cmpB ->
        pmatch cmpA $ \(PComparator fA) ->
            pmatch cmpB $ \(PComparator fB) ->
                pcon . PComparator . plam $ \x y ->
                    pmatch (discriminate # x) $ \case
                        PLeft xA -> pmatch (discriminate # y) $ \case
                            PLeft yA -> fA # xA # yA
                            PRight _ -> pcon PLT
                        PRight xB -> pmatch (discriminate # y) $ \case
                            PLeft _ -> pcon PGT
                            PRight yB -> fB # xB # yB

{- | Given a projection from a type to another type which we have a
 'PComparator' for, construct a new 'PComparator'.

 @since 3.4.0
-}
pmapComparator ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s ((b :--> a) :--> PComparator a :--> PComparator b)
pmapComparator = phoistAcyclic $
    plam $ \f cmp ->
        pmatch cmp $ \(PComparator g) ->
            pcon . PComparator . plam $ \x y ->
                g # (f # x) # (f # y)

{- | Reverses the ordering described by a 'PComparator'.

 @since 3.4.0
-}
preverseComparator ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> PComparator a)
preverseComparator = phoistAcyclic $
    plam $ \cmp ->
        pmatch cmp $ \(PComparator f) ->
            pcon . PComparator . plam $ \x y ->
                pmatch (f # x # y) $ \case
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
    plam $ \cmp x y ->
        pmatch cmp $ \(PComparator f) -> f # x # y

{- | Uses a 'PComparator' for an equality check.

 @since 3.4.0
-}
pequateBy ::
    forall (a :: S -> Type) (s :: S).
    Term s (PComparator a :--> a :--> a :--> PBool)
pequateBy = phoistAcyclic $
    plam $ \cmp x y ->
        pmatch cmp $ \(PComparator f) -> pcon PEQ #== (f # x # y)
