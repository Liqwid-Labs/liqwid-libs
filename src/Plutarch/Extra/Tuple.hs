{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.Tuple (pfstTuple, psndTuple) where

import Plutarch.Api.V1.Tuple (PTuple)

{- | Extract the first component of a 'PTuple'.

     @since 3.0.3
-}
pfstTuple ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PIsData a) =>
    Term s (PTuple a b :--> a)
pfstTuple = phoistAcyclic $ plam $ pfromData . (pfield @"_0" #)

{- | Extract the second component of a 'PTuple'.

     @since 3.0.3
-}
psndTuple ::
    forall (b :: S -> Type) (a :: S -> Type) (s :: S).
    (PIsData b) =>
    Term s (PTuple a b :--> b)
psndTuple = phoistAcyclic $ plam $ pfromData . (pfield @"_1" #)
