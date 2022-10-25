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
import Plutarch.Positive (PPositive)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx (fromData)

--------------------------------------------------------------------------------

{- | Combined multiply-truncate.

 @since 3.9.0
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
 truncating.

 @since 3.9.0
-}
mulDivTruncate ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
mulDivTruncate =
  phoistAcyclic $
    plam $ \x num denom ->
      pdiv # (num * x) # denom

{- | Combined divide-truncate.

 @since 3.9.0
-}
divTruncate ::
  forall (s :: S).
  Term s (PRational :--> PInteger :--> PInteger)
divTruncate =
  phoistAcyclic $
    plam $ \ex x -> unTermCont $ do
      (PRational num denom) <- pmatchC ex
      pure $ mulDivTruncate # x # pto denom # num

{- | Scale a 'PRational' up by a factor indicated by a 'PInteger',
 without reducing the fraction.

 = Note

 This merely \'defers\' the reduction until later, with possibly a (very)
 large numerator. Use this only in cases where you know that this won't
 cause a performance blow-up later.

 @since 3.9.0
-}
mulRational ::
  forall (s :: S).
  Term s (PInteger :--> PRational :--> PRational)
mulRational =
  phoistAcyclic $
    plam $ \x r -> unTermCont $ do
      (PRational num denom) <- pmatchC r
      pure $ pcon $ PRational (num * x) denom

{- | Scale a 'PRational' down by a factor indicated by a 'PInteger', without
 reducing the fraction.

 = Note

 This has the same performance caveats as 'mulRational'.

 @since 3.9.0
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

{- | Create a 'PRational' out of two 'PIntegers'. Will error if the denominator
 is zero.

 @since 3.9.0
-}
(#%) ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PRational
x #% y =
  ptoPositiveCases
    y
    (pcon . PRational (x * (-1)))
    (pcon . PRational x)

{- | Absolute 'PInteger' as 'PPositive', distinguishing the @< 0@ and @> 0@ cases.

 Will error on 0.
-}
ptoPositiveCases ::
  forall (s :: S) (r :: PType).
  Term s PInteger ->
  (Term s PPositive -> Term s r) ->
  (Term s PPositive -> Term s r) ->
  Term s r
ptoPositiveCases n contNeg contPos =
  pif
    (n #<= 0)
    ( pif
        (n #== 0)
        (ptraceError "ptoPositiveCases with 0")
        -- The PPositive constructor is not exported, so we need coercion
        (contNeg (punsafeCoerce $ -n))
    )
    (contPos (punsafeCoerce n))

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
