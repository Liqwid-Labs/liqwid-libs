{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Map.Sorted (pkeysEqual, punionWith) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    pcon,
    pfix,
    phoistAcyclic,
    plam,
    pto,
    unTermCont,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Api.V1.AssocMap (KeyGuarantees (..), PMap (PMap))
import Plutarch.Bool (PBool, PEq ((#==)), POrd, pif, (#<))
import Plutarch.Builtin (
    PAsData,
    PBuiltinList,
    PBuiltinPair,
    PIsData,
    pdata,
    pfromData,
    pfstBuiltin,
    ppairDataBuiltin,
    psndBuiltin,
 )
import Plutarch.Extra.Map (pkeys)
import Plutarch.Extra.TermCont (pletC)
import Plutarch.List (PListLike (..), plistEquals)

{- | / O(n) /. True if both maps have exactly the same keys.

    @since 1.1.0
-}
pkeysEqual ::
    forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s (PMap 'Sorted k a :--> PMap 'Sorted k b :--> PBool)
pkeysEqual =
    phoistAcyclic $
        plam $
            \((pkeys #) -> keysA)
             ((pkeys #) -> keysB) ->
                    plistEquals # keysA # keysB

{- | / O(n) /. Union two maps using a merge function on collisions.

    @since 1.1.0
-}
punionWith ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    (PIsData k, POrd k, PIsData v) =>
    Term
        s
        ( (v :--> v :--> v)
            :--> PMap 'Sorted k v
            :--> PMap 'Sorted k v
            :--> PMap 'Sorted k v
        )
punionWith =
    phoistAcyclic $
        plam $
            \merger
             (pto -> a :: Term _ (PBuiltinList _))
             (pto -> b :: Term _ (PBuiltinList _)) ->
                    pcon $ PMap $ merge # (mkMerger # merger) # a # b
  where
    mkMerger ::
        Term
            _
            ( (v :--> v :--> v)
                :--> PBuiltinPair (PAsData k) (PAsData v)
                :--> PBuiltinPair (PAsData k) (PAsData v)
                :--> PBuiltinPair (PAsData k) (PAsData v)
            )
    mkMerger = phoistAcyclic $
        plam $ \f a b ->
            let k = pfstBuiltin # a
                va = pfromData $ psndBuiltin # a
                vb = pfromData $ psndBuiltin # b
                v = pdata $ f # va # vb
             in ppairDataBuiltin # k # v

    merge ::
        Term
            _
            ( ( PBuiltinPair (PAsData k) (PAsData v)
                    :--> PBuiltinPair (PAsData k) (PAsData v)
                    :--> PBuiltinPair (PAsData k) (PAsData v)
              )
                :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
                :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
                :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
            )
    merge = phoistAcyclic $
        pfix #$ plam $ \self' merger a b ->
            pif (pnull # a) b $
                pif (pnull # b) a $
                    unTermCont $ do
                        self <- pletC $ self' # merger

                        ah <- pletC $ phead # a
                        at <- pletC $ ptail # a
                        bh <- pletC $ phead # b
                        bt <- pletC $ ptail # b

                        ahk <- pletC $ pfromData $ pfstBuiltin # ah
                        bhk <- pletC $ pfromData $ pfstBuiltin # bh

                        pure $
                            pif
                                (ahk #== bhk)
                                ( pcons # (merger # ah # bh)
                                    # (self # at # bt)
                                )
                                $ pif
                                    (ahk #< bhk)
                                    (pcons # ah #$ self # at # b)
                                    (pcons # bh #$ self # a # bt)
