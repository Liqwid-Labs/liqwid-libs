{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Numeric (peven, (#^)) where

import Plutarch.Extra.TermCont (pletC)
import Plutarch.Num (PNum)

-- | @since 1.0.0
peven ::
  forall (a :: S -> Type) (s :: S).
  (PIntegral a, PEq a, PNum a) =>
  Term s (a :--> PBool)
peven = phoistAcyclic $ plam $ \x -> (prem # x # 2) #== 0

ppowIntegral ::
  forall (s :: S) (n :: S -> Type) (e :: S -> Type).
  (PNum n, PNum e, PEq e, PIntegral e) =>
  Term s (n :--> e :--> n)
ppowIntegral =
  phoistAcyclic $
    plam $ \x n ->
      pif (n #== 0) 1 $ go # x # n
  where
    go = pfix #$ plam $ \self x n ->
      pif (n #== 1) x $
        unTermCont $ do
          next <- pletC $ self # (x * x) # (pdiv # n # 2)
          pure $
            pif
              (peven # n)
              next
              (x * next)

{- | Power to a 'PIntegral' exponent. Exponent must be @>= 0@.

 @since 3.12.1
-}
(#^) ::
  forall (s :: S) (n :: S -> Type) (e :: S -> Type).
  (PNum n, PNum e, PEq e, PIntegral e) =>
  Term s n ->
  Term s e ->
  Term s n
n #^ e = ppowIntegral # n # e

infixr 8 #^
