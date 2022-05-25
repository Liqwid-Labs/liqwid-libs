{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Numeric (peven) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    phoistAcyclic,
    plam,
    (#),
    type (:-->),
 )
import Plutarch.Bool (PBool, PEq ((#==)))
import Plutarch.Integer (PIntegral (prem))

-- | @since 1.0.0
peven ::
    forall (a :: S -> Type) (s :: S).
    (PIntegral a, PEq a, forall (s' :: S). Num (Term s' a)) =>
    Term s (a :--> PBool)
peven = phoistAcyclic $ plam $ \x -> (prem # x # 2) #== 0
