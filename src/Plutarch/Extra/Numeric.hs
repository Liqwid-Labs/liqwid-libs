{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Extra.Numeric (peven) where

import Data.Kind (Type)
import Plutarch.Num (PNum)
import Plutarch.Prelude (
    PBool,
    PEq ((#==)),
    PIntegral (prem),
    S,
    Term,
    phoistAcyclic,
    plam,
    (#),
    type (:-->),
 )

-- | @since 1.0.0
peven ::
    forall (a :: S -> Type) (s :: S).
    (PIntegral a, PEq a, PNum a) =>
    Term s (a :--> PBool)
peven = phoistAcyclic $ plam $ \x -> (prem # x # 2) #== 0
