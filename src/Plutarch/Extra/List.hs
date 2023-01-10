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
  pfromList,

  -- * Transformation
  pmapMaybe,
  pdeleteFirstBy,
  ptryDeleteFirstBy,
  pdeleteFirst,

  -- * Search
  pfindJust,
  plookupAssoc,

  -- * Elimination
  phandleList,
  precListLookahead,
  ptryElimSingle,

  -- * Comparison
  plistEqualsBy,

  -- * Singleton
  pisSingleton,
  pfromSingleton,
  ptryFromSingleton,
) where

import Plutarch.Extra.Functor (PFunctor (pfmap))
import Plutarch.Extra.Maybe (pjust, pnothing)

{- | 'pelimList' with re-ordered arguments. Useful for cases when the \'nil
 case\' is simple, but the \'cons case\' is complex.

 @since 3.9.0
-}
phandleList ::
  forall (a :: S -> Type) (r :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  Term s (ell a) ->
  Term s r ->
  (Term s a -> Term s (ell a) -> Term s r) ->
  Term s r
phandleList xs whenNil whenCons = pelimList whenCons whenNil xs

{- | Similar to 'pelimList', but assumes the argument list-like is a singleton,
 erroring otherwise.

 @since 3.9.0
-}
ptryElimSingle ::
  forall (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (r :: S -> Type) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  (Term s a -> Term s r) ->
  Term s (ell a) ->
  Term s r
ptryElimSingle f = pelimList go (ptraceError emptyErr)
  where
    go ::
      Term s a ->
      Term s (ell a) ->
      Term s r
    go h t = pif (pnull # t) (f h) (ptraceError nonSingleErr)
    emptyErr :: Term s PString
    emptyErr = "ptryElimSingle: Found empty list-like."
    nonSingleErr :: Term s PString
    nonSingleErr = "ptryElimSingle: Found non-singleton list-like."

{- | Similar to 'pmap', but allows elements to be thrown out. More precisely,
 for elements where the function argument returns 'PNothing', the
 corresponding element is removed, while for elements where the function
 argument returns `PJust`, the value is used in the result.

     @since 3.14.1
-}
pmapMaybe ::
  forall
    (listO :: (S -> Type) -> (S -> Type))
    (b :: S -> Type)
    (listI :: (S -> Type) -> (S -> Type))
    (a :: S -> Type)
    (s :: S).
  (PIsListLike listI a, PIsListLike listO b) =>
  Term s ((a :--> PMaybe b) :--> listI a :--> listO b)
pmapMaybe = phoistAcyclic $ plam $ \f -> precList (go f) (const pnil)
  where
    go ::
      forall (s' :: S).
      Term s' (a :--> PMaybe b) ->
      Term s' (listI a :--> listO b) ->
      Term s' a ->
      Term s' (listI a) ->
      Term s' (listO b)
    go f self x xs = pmatch (f # x) $ \case
      PNothing -> self # xs
      PJust y -> pcons # y #$ self # xs

{- | A combination of 'pmap' and 'pfind', but without needing an intermediate
 structure. More precisely, searched for the first element in a list-like
 structure that produces a 'PJust' argument, returning it if found; otherwise,
 produces 'PNothing'.

 @since 3.6.0
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
      PJust v -> pcon $ PJust v

{- | Treats a list-like structure as an assoc list. More precisely, given a
 list-like structure of key-value pairs, a method of extracting the key and
 the value, and a \'target\' key, returns the corresponding value, or
 'PNothing' if there isn't one.

 = Note

 There may be multiple mappings for a specific key; in such a situation, only
 the /first/ match is returned. In general, this requires time proportional to
 the length of the list-like structure, as we may have to check every entry.

 @since 3.6.0
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

 @since 3.6.0
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

-- Similar to 'precList', but with a \'look-ahead\' in the list-like structure
-- being eliminated. This is more efficient than repeated use of 'pelimList' (or
-- worse, 'puncons'). Furthermore, the \'self argument\' is not passed to the
-- \'nil\' and \'singleton\' cases, as it's pointless there.
--
-- @ since 3.6.0
precListLookahead ::
  forall (a :: S -> Type) (r :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
  (PElemConstraint ell a, PListLike ell) =>
  -- | The \'two or more\' case. First @'Term' s a@ is the \'head\', second is
  -- a \'peek-ahead\', while the @'Term' s (ell a)@ is what remains /after/
  -- the \'peek-ahead\'.
  (Term s (a :--> ell a :--> r) -> Term s a -> Term s a -> Term s (ell a) -> Term s r) ->
  -- | The \'singleton\' case, used both for true singletons and also for the
  -- end of a non-empty list-like.
  (Term s a -> Term s r) ->
  -- | The \'nil\' case.
  Term s r ->
  Term s (ell a :--> r)
precListLookahead whenContinuing whenOne whenDone =
  plam $
    pelimList (\x xs -> (pfix #$ plam $ go) # x # xs) whenDone
  where
    go ::
      Term s (a :--> ell a :--> r) ->
      Term s a ->
      Term s (ell a) ->
      Term s r
    go self h =
      pelimList
        (whenContinuing self h)
        (whenOne h)

{- | Turn a Haskell-level list of "Term"s into a "PListLike"
 @since 3.9.0
-}
pfromList ::
  forall (list :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PIsListLike list a) =>
  [Term s a] ->
  Term s (list a)
pfromList = foldr (\x xs -> pcons # x # xs) pnil

{- | Check if two lists are equal. The two list types can be different. The first
     argument is used for equality checks.

     @since 3.14.1
-}
plistEqualsBy ::
  forall
    (list1 :: (S -> Type) -> (S -> Type))
    (list2 :: (S -> Type) -> (S -> Type))
    (a :: S -> Type)
    (b :: S -> Type)
    (s :: S).
  (PIsListLike list1 a, PIsListLike list2 b) =>
  Term s ((a :--> b :--> PBool) :--> list1 a :--> list2 b :--> PBool)
plistEqualsBy = phoistAcyclic $
  plam $ \eq -> pfix #$ plam $ \self l1 l2 ->
    pelimList
      ( \x xs ->
          pelimList
            ( \y ys ->
                -- Avoid comparison if two lists have different length.
                self # xs # ys #&& eq # x # y
            )
            -- l2 is empty, but l1 is not.
            (pconstant False)
            l2
      )
      -- l1 is empty, so l2 should be empty as well.
      (pnull # l2)
      l1

{- | / O(n) /. Remove the first occurance of a value that satisfies the
     predicate from the given list. Return nothing if said value is not in
     the list.

     @since 3.14.1
-}
pdeleteFirstBy ::
  forall
    (a :: S -> Type)
    (list :: (S -> Type) -> (S -> Type))
    (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> PBool) :--> list a :--> PMaybe (list a))
pdeleteFirstBy = phoistAcyclic $
  plam $ \p ->
    precList
      ( \self h t ->
          pif
            (p # h)
            (pjust # t)
            (pfmap # (pcons # h) # (self # t))
      )
      (const pnothing)

{- | Partial version of 'pdeleteBy'.

     @since 3.14.1
-}
ptryDeleteFirstBy ::
  forall
    (a :: S -> Type)
    (list :: (S -> Type) -> (S -> Type))
    (s :: S).
  (PIsListLike list a) =>
  Term s ((a :--> PBool) :--> list a :--> list a)
ptryDeleteFirstBy = phoistAcyclic $
  plam $ \p ->
    precList
      ( \self h t ->
          pif
            (p # h)
            t
            (pcons # h #$ self # t)
      )
      (const $ ptraceError "Cannot delete element")

{- | Special version of 'pdeleteBy', for types with 'PEq' instance.

     @since 3.14.1
-}
pdeleteFirst ::
  forall
    (a :: S -> Type)
    (list :: (S -> Type) -> (S -> Type))
    (s :: S).
  (PEq a, PIsListLike list a) =>
  Term s (a :--> list a :--> PMaybe (list a))
pdeleteFirst = phoistAcyclic $ plam $ \x -> pdeleteFirstBy # plam (#== x)

{- | / O(1) /.Return true if the given list has exactly one element.

     @since 3.14.1
-}
pisSingleton ::
  forall
    (a :: S -> Type)
    (list :: (S -> Type) -> (S -> Type))
    (s :: S).
  (PIsListLike list a) =>
  Term s (list a :--> PBool)
pisSingleton =
  phoistAcyclic $
    precList
      (\_ _ t -> pnull # t)
      (const $ pconstant False)

{- | Extract a singeton value from the given list. Return nothing if the list
     consists of zero or more than one elements.

     @since 3.14.1
-}
pfromSingleton ::
  forall
    (a :: S -> Type)
    (list :: (S -> Type) -> (S -> Type))
    (s :: S).
  (PIsListLike list a) =>
  Term s (list a :--> PMaybe a)
pfromSingleton =
  phoistAcyclic $
    precList
      ( \_ h t ->
          pif
            (pnull # t)
            (pjust # h)
            pnothing
      )
      (const pnothing)

{- | Partial version of `pfromSingleton`.

     @since 3.14.1
-}
ptryFromSingleton ::
  forall
    (a :: S -> Type)
    (list :: (S -> Type) -> (S -> Type))
    (s :: S).
  (PIsListLike list a) =>
  Term s (list a :--> a)
ptryFromSingleton =
  phoistAcyclic $
    precList
      ( \_ h t ->
          pif
            (pnull # t)
            h
            (ptraceError "More than one element")
      )
      (const $ ptraceError "Empty list")
