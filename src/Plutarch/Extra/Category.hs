module Plutarch.Extra.Category (
  PSemigroupoid ((#>>>)),
  PCategory (pidentity),
  pconst,
  (#<<<),
) where

import Plutarch.Extra.Profunctor (PProfunctor (PCoSubcategory, PContraSubcategory, prmap))

-- | @since 1.0.0
class (PProfunctor p) => PSemigroupoid (p :: (S -> Type) -> (S -> Type) -> S -> Type) where
  (#>>>) ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    ( PContraSubcategory p a
    , PContraSubcategory p b
    , PCoSubcategory p b
    , PCoSubcategory p c
    ) =>
    Term s (p a b) ->
    Term s (p b c) ->
    Term s (p a c)

-- | @since 1.0.0
instance PSemigroupoid (:-->) where
  (#>>>) ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    Term s (a :--> b) ->
    Term s (b :--> c) ->
    Term s (a :--> c)
  t #>>> t' = go # t # t'
    where
      go ::
        forall (s' :: S).
        Term s' ((a :--> b) :--> (b :--> c) :--> (a :--> c))
      go = phoistAcyclic $
        plam $ \ab ->
          plam $ \bc ->
            plam $ \x -> bc # (ab # x)

-- | @since 1.0.0
class (PSemigroupoid p) => PCategory (p :: (S -> Type) -> (S -> Type) -> S -> Type) where
  pidentity ::
    forall (a :: S -> Type) (s :: S).
    ( PContraSubcategory p a
    , PCoSubcategory p a
    ) =>
    Term s (p a a)

-- | @since 1.0.0
instance PCategory (:-->) where
  pidentity = phoistAcyclic $ plam id

-- | @since 1.0.0
pconst ::
  forall
    (p :: (S -> Type) -> (S -> Type) -> S -> Type)
    (a :: S -> Type)
    (b :: S -> Type)
    (s :: S).
  ( PContraSubcategory p b
  , PCategory p
  , PCoSubcategory p b
  , PCoSubcategory p a
  ) =>
  Term s (a :--> p b a)
pconst = phoistAcyclic $ plam $ \x -> prmap # (go # x) # pidentity
  where
    go :: forall (s' :: S). Term s' (a :--> b :--> a)
    go = phoistAcyclic $ plam const

-- | @since 1.0.0
(#<<<) ::
  forall
    (p :: (S -> Type) -> (S -> Type) -> S -> Type)
    (a :: S -> Type)
    (b :: S -> Type)
    (c :: S -> Type)
    (s :: S).
  ( PSemigroupoid p
  , PContraSubcategory p a
  , PContraSubcategory p b
  , PCoSubcategory p b
  , PCoSubcategory p c
  ) =>
  Term s (p b c) ->
  Term s (p a b) ->
  Term s (p a c)
t #<<< t' = t' #>>> t

infixr 1 #<<<
