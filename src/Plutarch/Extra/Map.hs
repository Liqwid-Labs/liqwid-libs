{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Extra.Map
 Description: Duplicated functions from `liqwid-plutarch-extra`

 Commit: d8be5f8dc91ad00158727fdbccb6db849e9f3559

 These functions are included here to avoid cyclic import issues.
 They should mirror the LPE implementations exactly.

 @since 2.0
-}
module Plutarch.Extra.Map (pkvPairLt) where

import Plutarch.Api.V1.AssocMap (KeyGuarantees, PMap (PMap))
import Plutarch.Builtin (pfstBuiltin, ppairDataBuiltin, psndBuiltin)
import qualified Plutarch.List
import Plutarch.Prelude (
    PAsData,
    PBool,
    PBuiltinList,
    PBuiltinPair,
    PEq,
    PIsData,
    PIsListLike,
    PMaybe (PJust, PNothing),
    POrd,
    S,
    Term,
    Type,
    pcon,
    pcons,
    pdata,
    pfix,
    pfromData,
    phead,
    phoistAcyclic,
    pif,
    plam,
    plet,
    pmatch,
    pnil,
    pnull,
    precList,
    psingleton,
    ptail,
    pto,
    unTermCont,
    (#),
    (#$),
    (#<),
    (#==),
    (:-->),
 )

-- | Compare two key-value pairs by their keys, return true if the first key is less than the second one.
pkvPairLt ::
    (PIsData k, POrd k) =>
    Term s (PBuiltinPair (PAsData k) (PAsData v) :--> PBuiltinPair (PAsData k) (PAsData v) :--> PBool)
pkvPairLt = phoistAcyclic $
    plam $ \((pkvPairKey #) -> keyA) ((pkvPairKey #) -> keyB) -> keyA #< keyB

-- | Get the key of a key-value pair.
pkvPairKey :: (PIsData k) => Term s (PBuiltinPair (PAsData k) (PAsData v) :--> k)
pkvPairKey = phoistAcyclic $ plam $ \(pfromData . (pfstBuiltin #) -> key) -> key

-- | / O(n) /. Map a function over all values in a 'PMap'.
pmap ::
    forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (keys :: KeyGuarantees) (s :: S).
    (PIsData k, PIsData a, PIsData b) =>
    Term s ((a :--> b) :--> PMap keys k a :--> PMap keys k b)
pmap = phoistAcyclic $
    plam $ \f (pto -> (ps :: Term s' (PBuiltinList a'))) ->
        pcon $
            PMap $
                Plutarch.List.pmap
                    # plam
                        ( \kv ->
                            let k = pfstBuiltin # kv
                                v = psndBuiltin # kv

                                nv = pdata $ f # pfromData v
                             in ppairDataBuiltin # k # nv
                        )
                    # ps
