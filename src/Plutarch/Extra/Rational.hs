-- TODO: Remove this and replace with TermCont.
{-# LANGUAGE QualifiedDo #-}
-- TODO: Either disable warning about orphans altogether or address the issue
-- requiring this unusual solution.
{-# OPTIONS_GHC -Wwarn=orphans #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.Rational (
    mulTruncate,
    mulDivTruncate,
    divTruncate,
    mulRational,
    divRational,
    pliftTaggedRational,
) where

-------------------------------------------------------------------------------

import GHC.Stack (HasCallStack)
import qualified Plutarch.Monadic as P
import Plutarch.Positive (ptryPositive)
import PlutusTx (fromData)
import Plutarch.Extra.Tagged (PTagged)
import Data.Tagged (Tagged)
import Data.Maybe (fromJust)
import Plutarch.Builtin (pforgetData)
import Plutarch.Orphans ()

--------------------------------------------------------------------------------

-- | Combined multiply-truncate
mulTruncate ::
    forall (s :: S).
    Term s (PRational :--> PInteger :--> PInteger)
mulTruncate =
    phoistAcyclic $
        plam $ \ex x -> P.do
            (PRational num denom) <- pmatch ex
            mulDivTruncate # x # num # pto denom

{- | Multiply the first argument by the second argument, divide by the third,
 truncating
-}
mulDivTruncate ::
    forall (s :: S).
    Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
mulDivTruncate =
    phoistAcyclic $
        plam $ \x num denom -> P.do
            pdiv # (num * x) # denom

-- | Combined divide-truncate
divTruncate ::
    forall (s :: S).
    Term s (PRational :--> PInteger :--> PInteger)
divTruncate =
    phoistAcyclic $
        plam $ \ex x -> P.do
            (PRational num denom) <- pmatch ex
            mulDivTruncate # x # pto denom # num

-- | Multiply a Rational by an Integer, without reducing the fraction
mulRational ::
    forall (s :: S).
    Term s (PInteger :--> PRational :--> PRational)
mulRational =
    phoistAcyclic $
        plam $ \x r -> P.do
            (PRational num denom) <- pmatch r
            pcon $ PRational (num * x) denom

-- | Multiply a Rational by an Integer, without reducing the fraction
divRational ::
    forall (s :: S).
    Term s (PInteger :--> PRational :--> PRational)
divRational =
    phoistAcyclic $
        plam $ \x r -> P.do
            (PRational num denom) <- pmatch r
            (pto denom * x) #% num

{- | Create a `PRational` out of two `PIntegers`. Will error if the denominator
 is  non-positive.
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
