{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Extra.Map
 Description: Duplicated functions from `liqwid-plutarch-extra`

 Commit: d8be5f8dc91ad00158727fdbccb6db849e9f3559

 These functions are included here to avoid cyclic import issues.
 They should mirror the LPE implementations exactly.

 @since 2.0
-}
module Plutarch.Extra.Map (pkvPairLt) where

import Plutarch.Builtin (pfstBuiltin)
import Plutarch.Prelude (
    PAsData,
    PBool,
    PBuiltinPair,
    PIsData,
    POrd,
    Term,
    pfromData,
    phoistAcyclic,
    plam,
    (#),
    (#<),
    (:-->),
 )

{- | Compare two key-value pairs by their keys, return true if the first key is
 less than the second one.
-}
pkvPairLt ::
    (PIsData k, POrd k) =>
    Term
        s
        ( PBuiltinPair (PAsData k) (PAsData v)
            :--> PBuiltinPair (PAsData k) (PAsData v)
            :--> PBool
        )
pkvPairLt = phoistAcyclic $
    plam $ \((pkvPairKey #) -> keyA) ((pkvPairKey #) -> keyB) ->
        keyA #< keyB

-- | Get the key of a key-value pair.
pkvPairKey ::
    (PIsData k) =>
    Term
        s
        ( PBuiltinPair
            (PAsData k)
            (PAsData v)
            :--> k
        )
pkvPairKey = phoistAcyclic $
    plam $ \(pfromData . (pfstBuiltin #) -> key) ->
        key
