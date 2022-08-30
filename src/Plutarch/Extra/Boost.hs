{- |
 Module: Plutarch.Extra.Boost
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Performance-oriented metaprogramming helpers.
-}
module Plutarch.Extra.Boost (
    -- * Unhoisted list helpers
    psing,
    plist,

    -- * CPS-driven matching
    pmatchList,
) where

{- | Make a list-like structure from any 'Foldable' of 'Term's. This is
 unhoisted.

 @since 3.4.0
-}
plist ::
    forall (ell :: (S -> Type) -> S -> Type) (f :: Type -> Type) (a :: S -> Type) (s :: S).
    (Foldable f, PListLike ell, PElemConstraint ell a) =>
    f (Term s a) ->
    Term s (ell a)
plist = foldr (\x xs -> pcons # x # xs) pnil

{- | Make a singleton list-like. This is unhoisted.

 @since 3.4.0
-}
psing ::
    forall (ell :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    Term s a ->
    Term s (ell a)
psing x = plist [x]

{- | An \'uncons\' operation, designed for CPS chaining, for arbitrary list-like
 structures.

 @since 3.4.0
-}
pmatchList ::
    forall (r :: S -> Type) (a :: S -> Type) (ell :: (S -> Type) -> S -> Type) (s :: S).
    (PElemConstraint ell a, PListLike ell) =>
    -- | Continuation for \'nil\'.
    Term s r ->
    -- | Continuation for \'cons\'.
    (Term s a -> Term s (ell a) -> Term s r) ->
    -- | The list-like to match on.
    Term s (ell a) ->
    Term s r
pmatchList = flip pelimList
