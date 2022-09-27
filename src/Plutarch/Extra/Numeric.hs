{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Numeric (peven) where

import Plutarch.Num (PNum)

-- | @since 1.0.0
peven ::
  forall (a :: S -> Type) (s :: S).
  (PIntegral a, PEq a, PNum a) =>
  Term s (a :--> PBool)
peven = phoistAcyclic $ plam $ \x -> (prem # x # 2) #== 0
