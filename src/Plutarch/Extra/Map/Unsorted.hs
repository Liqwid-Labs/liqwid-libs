{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Extra.Map.Unsorted

 Description: Duplicated functions from `liqwid-plutarch-extra`

 Commit: d8be5f8dc91ad00158727fdbccb6db849e9f3559

 These functions are included here to avoid cyclic import issues.
 They should mirror the LPE implementations exactly.

 @since 2.0
-}
module Plutarch.Extra.Map.Unsorted (psort) where

import Plutarch.Api.V1.AssocMap (KeyGuarantees (Sorted, Unsorted), PMap (PMap))
import Plutarch.Extra.List (pmsortBy)
import Plutarch.Extra.Map (pkvPairLt)
import Plutarch.Prelude (
    PBuiltinList,
    PIsData,
    POrd,
    S,
    Term,
    Type,
    pcon,
    phoistAcyclic,
    plam,
    pto,
    (#),
    (:-->),
 )

{- | / O(nlogn) /. Sort a `PMap` by the keys of each key-value pairs, in an ascending order.

 @since 2.0
-}
psort ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    (PIsData k, POrd k) =>
    Term s (PMap 'Unsorted k v :--> PMap 'Sorted k v)
psort = phoistAcyclic $
    plam $ \(pto -> l :: (Term s' (PBuiltinList a))) ->
        pcon $ PMap $ pmsortBy # pkvPairLt # l
