{- |
 Module: Plutarch.Extra.List
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Various helpers for list-like structures. This module is intended to be
 imported qualified.
-}
module Plutarch.Extra.List (
    -- * Construction
    preplicate,

    -- * Predicates
    pnotNull,

    -- * Transformation
    pmapMaybe,

    -- * Search
    pfind',
    pfindJust,
    plookupAssoc,
) where

import qualified Plutarch.List as PList

{- | Returns 'PTrue' if the list-like structure contains at least one element.

 @since 3.4.0
-}
pnotNull ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PIsListLike ell a) =>
    Term s (ell a :--> PBool)
pnotNull = phoistAcyclic $ plam $ \xs -> pnot #$ pnull # xs

{- | Similar to 'pmap', but allows elements to be thrown out. More precisely,
 for elements where the function argument returns 'PNothing', the
 corresponding element is removed, while for elements where the function
 argument returns `PJust`, the value is used in the result.

 @since 3.4.0
-}
pmapMaybe ::
    forall (b :: S -> Type) (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PElemConstraint ell a, PElemConstraint ell b, PListLike ell) =>
    Term s ((a :--> PMaybe b) :--> ell a :--> ell b)
pmapMaybe = phoistAcyclic $
    pfix #$ plam $ \self f xs ->
        pmatch (PList.puncons # xs) $ \case
            PNothing -> pnil
            PJust xs' -> pmatch xs' $ \(PPair h t) ->
                pmatch (f # h) $ \case
                    PNothing -> self # f # t
                    PJust h' -> pcons # h' # (self # f # t)

{- | As 'pfind', but with a Haskell-level function on 'Term's for the predicate.

 @since 3.4.0
-}
pfind' ::
    forall (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    (Term s a -> Term s PBool) ->
    Term s (ell a :--> PMaybe a)
pfind' p =
    precList
        (\self x xs -> pif (p x) (pcon $ PJust x) (self # xs))
        (const $ pcon PNothing)

{- | A combination of 'pmap' and 'pfind', but without needing an intermediate
 structure. More precisely, searched for the first element in a list-like
 structure that produces a 'PJust' argument, returning it if found; otherwise,
 produces 'PNothing'.

 @since 3.4.0
-}
pfindJust ::
    forall (b :: S -> Type) (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s ((a :--> PMaybe b) :--> ell a :--> PMaybe b)
pfindJust = phoistAcyclic $ plam $ \f -> precList (go f) (const $ pcon PNothing)
  where
    go ::
        forall (s' :: S).
        Term s' (a :--> PMaybe b) ->
        Term s' (ell a :--> PMaybe b) ->
        Term s' a ->
        Term s' (ell a) ->
        Term s' (PMaybe b)
    go f self x xs = pmatch (f # x) $ \case
        PNothing -> self # xs
        PJust v -> pcon . PJust $ v

{- | Treats a list-like structure as an assoc list. More precisely, given a
 list-like structure of key-value pairs, a method of extracting the key and
 the value, and a \'target\' key, returns the corresponding value, or
 'PNothing' if there isn't one.

 = Note

 There may be multiple mappings for a specific key; in such a situation, only
 the /first/ match is returned. In general, this requires time proportional to
 the length of the list-like structure, as we may have to check every entry.

 @since 3.4.0
-}
plookupAssoc ::
    forall (k :: S -> Type) (v :: S -> Type) (kv :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell kv, PListLike ell, PEq k) =>
    Term s ((kv :--> k) :--> (kv :--> v) :--> k :--> ell kv :--> PMaybe v)
plookupAssoc = phoistAcyclic $
    plam $ \getKey getVal target kvs ->
        pmatch (pfindJust # (go # getKey # target) # kvs) $ \case
            PNothing -> pcon PNothing
            PJust kv -> pcon . PJust $ getVal # kv
  where
    go ::
        forall (s' :: S).
        Term s' ((kv :--> k) :--> k :--> kv :--> PMaybe kv)
    go = phoistAcyclic $
        plam $ \getKey target kv ->
            pif
                (target #== (getKey # kv))
                (pcon . PJust $ kv)
                (pcon PNothing)

{- | Given a count @n@ and a value @x@, produces a list-like structure
 containing @n@ copies of @x@, or an empty structure if @n@ is non-positive.

 = Note

 You will likely need to specify which list-like structure you want; the type
 arguments for this function are optimized for use with `TypeApplications` to
 do exactly this task.

 @since 3.4.0
-}
preplicate ::
    forall (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s (PInteger :--> a :--> ell a)
preplicate = phoistAcyclic $
    pfix #$ plam $ \self count x ->
        pif
            (count #<= 0)
            pnil
            (pcons # x # (self # (count - 1) # x))
