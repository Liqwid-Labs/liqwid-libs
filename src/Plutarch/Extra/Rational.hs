{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.Rational (
    mulTruncate,
    mulDivTruncate,
    divTruncate,
    mulRational,
    divRational,
    pliftTaggedRational,
    (#%),
) where

-------------------------------------------------------------------------------

import Data.Maybe (fromJust)
import Data.Tagged (Tagged)
import GHC.Stack (HasCallStack)
import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.Tagged (PTagged)
import "plutarch-extra" Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Orphans ()
import Plutarch.Positive (ptryPositive)
import PlutusTx (fromData)

--------------------------------------------------------------------------------

{- | Combined multiply-truncate

 @since 3.8.0
-}
mulTruncate ::
    forall (s :: S).
    Term s (PRational :--> PInteger :--> PInteger)
mulTruncate =
    phoistAcyclic $
        plam $ \ex x -> unTermCont $ do
            (PRational num denom) <- pmatchC ex
            pure $ mulDivTruncate # x # num # pto denom

{- | Multiply the first argument by the second argument, divide by the third,
 truncating

 @since 3.8.0
-}
mulDivTruncate ::
    forall (s :: S).
    Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
mulDivTruncate =
    phoistAcyclic $
        plam $ \x num denom ->
            pdiv # (num * x) # denom

{- | Combined divide-truncate

 @since 3.8.0
-}
divTruncate ::
    forall (s :: S).
    Term s (PRational :--> PInteger :--> PInteger)
divTruncate =
    phoistAcyclic $
        plam $ \ex x -> unTermCont $ do
            (PRational num denom) <- pmatchC ex
            pure $ mulDivTruncate # x # pto denom # num

{- | Multiply a "PInteger" by a "PInteger", without reducing the fraction

 NOTE: this can cause performance issues if you're not careful.

 @since 3.8.0
-}
mulRational ::
    forall (s :: S).
    Term s (PInteger :--> PRational :--> PRational)
mulRational =
    phoistAcyclic $
        plam $ \x r -> unTermCont $ do
            (PRational num denom) <- pmatchC r
            pure $ pcon $ PRational (num * x) denom

{- | Divide a "PInteger" by a "PRational", without reducing the fraction

 NOTE: this can cause performance issues if you're not careful.

 @since 3.8.0
-}
divRational ::
    forall (s :: S).
    Term s (PInteger :--> PRational :--> PRational)
divRational =
    phoistAcyclic $
        plam $ \x r -> unTermCont $ do
            (PRational num denom) <- pmatchC r
            pure $ (pto denom * x) #% num

infixl 7 #%

{- | Create a `PRational` out of two `PIntegers`. Will error if the denominator
 is  non-positive.

 @since 3.8.0
-}
(#%) ::
    forall (s :: S).
    Term s PInteger ->
    Term s PInteger ->
    Term s PRational
x #% y =
    pif
        (y #< 0)
        (pcon $ PRational (x * (-1)) (ptryPositive # (y * (-1))))
        (pcon $ PRational x (ptryPositive # y))

-- | `plift` for Tagged Rationals (kind polymorphic)
pliftTaggedRational ::
    forall k (tag :: k).
    HasCallStack =>
    ClosedTerm (PTagged tag PRational) ->
    Tagged tag Rational
pliftTaggedRational term =
    fromJust $
        PlutusTx.fromData $
            plift (pforgetData $ pdata term)
