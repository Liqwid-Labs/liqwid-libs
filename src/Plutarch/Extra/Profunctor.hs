{-# LANGUAGE TypeFamilies #-}

module Plutarch.Extra.Profunctor (
    PProfunctor (..),
) where

import Data.Kind (Constraint, Type)
import Generics.SOP (Top)
import Plutarch (S, Term, phoistAcyclic, plam, (#), type (:-->))
import Plutarch.Extra.Function (pidentity)

-- | @since 1.0.0
class PProfunctor (p :: (S -> Type) -> (S -> Type) -> S -> Type) where
    {-# MINIMAL pdimap | plmap, prmap #-}
    type PContraSubcategory p :: (S -> Type) -> Constraint
    type PCoSubcategory p :: (S -> Type) -> Constraint
    pdimap ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type) (s :: S).
        ( PContraSubcategory p a
        , PContraSubcategory p b
        , PCoSubcategory p c
        , PCoSubcategory p d
        ) =>
        Term s ((a :--> b) :--> (c :--> d) :--> p b c :--> p a d)
    pdimap = phoistAcyclic $ plam $ \f g p -> plmap # f # (prmap # g # p)
    plmap ::
        forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
        ( PContraSubcategory p a
        , PContraSubcategory p b
        , PCoSubcategory p c
        ) =>
        Term s ((a :--> b) :--> p b c :--> p a c)
    plmap = phoistAcyclic $ plam $ \f p -> pdimap # f # pidentity # p
    prmap ::
        forall (a :: S -> Type) (c :: S -> Type) (d :: S -> Type) (s :: S).
        ( PContraSubcategory p a
        , PCoSubcategory p c
        , PCoSubcategory p d
        ) =>
        Term s ((c :--> d) :--> p a c :--> p a d)
    prmap = phoistAcyclic $ plam $ \g p -> pdimap # pidentity # g # p

-- | @since 1.0.0
instance PProfunctor (:-->) where
    type PContraSubcategory (:-->) = Top
    type PCoSubcategory (:-->) = Top
    pdimap = phoistAcyclic $ plam $ \ab cd bc -> plam $ \x -> cd # (bc # (ab # x))
