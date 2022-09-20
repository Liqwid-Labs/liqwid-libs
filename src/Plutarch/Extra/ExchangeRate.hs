module Plutarch.Extra.ExchangeRate (
    (:>),

    -- * Exchange-rate conversions
    exchangeFromTruncate,
    exchangeToTruncate,
    exchangeFrom,
    exchangeTo,
) where

import GHC.TypeLits (Symbol)

import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.Rational (divRational, divTruncate, mulRational, mulTruncate)
import Plutarch.Extra.Tagged (PTagged)

{- | Represents an exchange from a to b.
     Let's say 1.00 ADA is worth 2.00 USD, then @ADA ':>' USD@ ought to be
     represented as 2.00.
-}
data (:>) (a :: Symbol) (b :: Symbol)

-- | Exchange from one currency to another, truncating the result
exchangeFromTruncate ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged a PInteger
            :--> PTagged b PInteger
        )
exchangeFromTruncate =
    phoistAcyclic $
        plam $ \ex x -> ppure #$ mulTruncate # pto ex # pto x

{- | Exchange from  one currency to another, truncating the result
 (inverse direction)
-}
exchangeToTruncate ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged b PInteger
            :--> PTagged a PInteger
        )
exchangeToTruncate =
    phoistAcyclic $
        plam $ \ex x -> ppure #$ divTruncate # pto ex # pto x

-- | Convert between quantities of currencies using a Rational conversion value
exchangeFrom ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged a PInteger
            :--> PTagged b PRational
        )
exchangeFrom =
    phoistAcyclic $
        plam $ \ex x -> ppure #$ mulRational # pto x # pto ex

-- | Convert between quantities of currencies, in the inverse direction
exchangeTo ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged b PInteger
            :--> PTagged a PRational
        )
exchangeTo =
    phoistAcyclic $
        plam $ \ex x ->
            ppure #$ divRational # pto x # pto ex
