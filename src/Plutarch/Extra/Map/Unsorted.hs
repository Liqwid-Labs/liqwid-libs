{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.Map.Unsorted (
    psort,
    pkeysEqual,
    punionWith,
) where

import Plutarch.Api.V1.AssocMap (KeyGuarantees (Sorted, Unsorted), PMap (PMap))
import Plutarch.Extra.List (pmsortBy)
import Plutarch.Extra.Map (pkeys, pkvPairLt)
import qualified Plutarch.Extra.Map.Sorted as SortedMap
import Plutarch.Extra.TermCont (pletC)

{- | / O(nlogn) /. Sort a `PMap` by the keys of each key-value pairs, in an ascending order.

    @since 1.1.0
-}
psort ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    (PIsData k, POrd k) =>
    Term s (PMap 'Unsorted k v :--> PMap 'Sorted k v)
psort = phoistAcyclic $
    plam $ \(pto -> l :: (Term _ (PBuiltinList _))) ->
        pcon $ PMap $ pmsortBy # pkvPairLt # l

{- | / O(nlogn) /. True if both maps have exactly the same keys.
     Using @'#=='@ is not sufficient, because keys returned are not ordered.

    @since 1.1.0
-}
pkeysEqual ::
    forall (k :: S -> Type) (a :: S -> Type) (b :: S -> Type) (s :: S).
    (POrd k, PIsData k) =>
    Term s (PMap 'Unsorted k a :--> PMap 'Unsorted k b :--> PBool)
pkeysEqual = phoistAcyclic $
    plam $ \p q -> unTermCont $ do
        pks <- pletC $ pkeys # p
        qks <- pletC $ pkeys # q

        pure $
            pif
                (plength # pks #== plength # qks)
                ( unTermCont $ do
                    let comp = phoistAcyclic $ plam $ \(pfromData -> x) (pfromData -> y) -> x #< y
                        spks = pmsortBy # comp # pks
                        sqks = pmsortBy # comp # qks

                    pure $ plistEquals # spks # sqks
                )
                (pcon PFalse)

{- | / O(nlogn) /. Union two maps using a merge function on collisions.

    @since 1.1.0
-}
punionWith ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    (PIsData k, POrd k, PIsData v) =>
    Term
        s
        ( (v :--> v :--> v)
            :--> PMap 'Unsorted k v
            :--> PMap 'Unsorted k v
            :--> PMap 'Sorted k v
        )
punionWith = phoistAcyclic $
    plam $ \f a b ->
        let sa = psort # a
            sb = psort # b
         in SortedMap.punionWith # f # sa # sb
