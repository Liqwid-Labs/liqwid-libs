{-# LANGUAGE PackageImports #-}

{- | Module: Plutarch.Extra.List
 Description: Duplicated functions from `liqwid-plutarch-extra`

 Commit: d8be5f8dc91ad00158727fdbccb6db849e9f3559

 These functions are included here to avoid cyclic import issues.
 They should mirror the LPE implementations exactly.

 @since 2.0
-}
module Plutarch.Extra.List (plookup, pfind', pmsortBy) where

import Plutarch.Builtin (pfstBuiltin, psndBuiltin)
import "plutarch-extra" Plutarch.Extra.TermCont (pletC)
import qualified Plutarch.List
import Plutarch.Prelude (
    PBool,
    PBuiltinPair,
    PEq,
    PIsListLike,
    PMaybe (PJust, PNothing),
    S,
    Term,
    Type,
    pcon,
    pcons,
    pfix,
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
    unTermCont,
    (#),
    (#$),
    (#==),
    (:-->),
 )

{- | /O(n)/. Find the value for a given key in an associative list.

 @since 2.0
-}
plookup ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S) list.
    (PEq a, PIsListLike list (PBuiltinPair a b)) =>
    Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
    phoistAcyclic $
        plam $ \k xs ->
            pmatch (pfind' (\p -> pfstBuiltin # p #== k) # xs) $ \case
                PNothing -> pcon PNothing
                PJust p -> pcon (PJust (psndBuiltin # p))

{- | Get the first element that matches a predicate or return Nothing.

 @since 2.0
-}
pfind' ::
    forall (a :: S -> Type) (s :: S) list.
    PIsListLike list a =>
    (Term s a -> Term s PBool) ->
    Term s (list a :--> PMaybe a)
pfind' p =
    precList
        (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
        (const $ pcon PNothing)

{- | / O(nlogn) /. Merge sort, bottom-up version, given a custom comparator.

   Assuming the comparator returns true if first value is less than the second
   one, the list elements will be arranged in ascending order, keeping
   duplicates in the order they appeared in the input.
-}
pmsortBy ::
    forall s a l.
    (PIsListLike l a, PIsListLike l (l a)) =>
    Term s ((a :--> a :--> PBool) :--> l a :--> l a)
pmsortBy = phoistAcyclic $
    plam $ \comp xs ->
        mergeAll # comp # (Plutarch.List.pmap # psingleton # xs)
  where
    mergeAll :: Term s' ((a :--> a :--> PBool) :--> l (l a) :--> l a)
    mergeAll = phoistAcyclic $
        pfix #$ plam $ \self comp xs ->
            pif (pnull # xs) pnil $
                let y = phead # xs
                    ys = ptail # xs
                 in pif (pnull # ys) y $
                        self # comp #$ mergePairs # comp # xs
    mergePairs :: Term s' ((a :--> a :--> PBool) :--> l (l a) :--> l (l a))
    mergePairs = phoistAcyclic $
        pfix #$ plam $ \self comp xs ->
            pif (pnull # xs) pnil $
                let y = phead # xs
                 in plet (ptail # xs) $ \ys ->
                        pif (pnull # ys) xs $
                            let z = phead # ys
                                zs = ptail # ys
                             in pcons # (pmergeBy # comp # y # z)
                                    # (self # comp # zs)

{- | / O(n) /. Merge two lists which are assumed to be ordered, given a custom
  comparator. The comparator should return true if first value is less than the
  second one.
-}
pmergeBy ::
    forall (a :: S -> Type) (s :: S) list.
    (PIsListLike list a) =>
    Term
        s
        ( (a :--> a :--> PBool)
            :--> list a
            :--> list a
            :--> list a
        )
pmergeBy = phoistAcyclic $ pfix #$ plam go
  where
    go self comp a b =
        pif (pnull # a) b $
            pif (pnull # b) a $
                unTermCont $ do
                    ah <- pletC $ phead # a
                    at <- pletC $ ptail # a
                    bh <- pletC $ phead # b
                    bt <- pletC $ ptail # b

                    pure $
                        pif
                            (comp # ah # bh)
                            (pcons # ah #$ self # comp # at # b)
                            (pcons # bh #$ self # comp # a # bt)
