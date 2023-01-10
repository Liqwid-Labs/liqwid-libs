module Plutarch.Extra.ExchangeRate (
  type (:>),

  -- * Exchange-rate conversions
  exchangeFromTruncate,
  exchangeToTruncate,
  exchangeFrom,
  exchangeTo,
) where

import GHC.TypeLits (Symbol)
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.Comonad (pextract)
import Plutarch.Extra.Rational (divRational, divTruncate, mulRational, mulTruncate)
import Plutarch.Extra.Tagged (PTagged)

{- | Represents an exchange from @a@ to @b@.

     For example, suppose 1.00 ADA is worth 2.00 USD. Then @ADA ':>' USD@
     represents scaling by 2.0.

   @since 3.9.0
-}
data (:>) (a :: Symbol) (b :: Symbol)

{- | Exchange from one currency to another, truncating the result.

 @since 3.9.0
-}
exchangeFromTruncate ::
  forall (a :: Symbol) (b :: Symbol) (s :: S).
  Term
    s
    ( PTagged (a :> b) PRational
        :--> PTagged a PInteger
        :--> PTagged b PInteger
    )
exchangeFromTruncate =
  phoistAcyclic $
    plam $
      \ex x -> ppure #$ mulTruncate # pto ex # pto x

{- | Exchange from  one currency to another, truncating the result
 (inverse direction).

 @since 3.9.0
-}
exchangeToTruncate ::
  forall (a :: Symbol) (b :: Symbol) (s :: S).
  Term
    s
    ( PTagged (a :> b) PRational
        :--> PTagged b PInteger
        :--> PTagged a PInteger
    )
exchangeToTruncate =
  phoistAcyclic $
    plam $ \ex x ->
      ppure #$ divTruncate # (pextract # ex) # (pextract # x)

{- | Convert between quantities of currencies using a 'PRational' conversion
  value.

 @since 3.9.0
-}
exchangeFrom ::
  forall (a :: Symbol) (b :: Symbol) (s :: S).
  Term
    s
    ( PTagged (a :> b) PRational
        :--> PTagged a PInteger
        :--> PTagged b PRational
    )
exchangeFrom =
  phoistAcyclic $
    plam $ \ex x ->
      ppure #$ mulRational # (pextract # x) # (pextract # ex)

{- | Convert between quantities of currencies, in the inverse direction.

 @since 3.9.0
-}
exchangeTo ::
  forall (a :: Symbol) (b :: Symbol) (s :: S).
  Term
    s
    ( PTagged (a :> b) PRational
        :--> PTagged b PInteger
        :--> PTagged a PRational
    )
exchangeTo =
  phoistAcyclic $
    plam $ \ex x ->
      ppure #$ divRational # (pextract # x) # (pextract # ex)
