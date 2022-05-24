module Plutarch.Extra.Function (
    pconst,
    pidentity,
    papply,
) where

import Data.Kind (Type)
import Plutarch (S, Term, phoistAcyclic, plam, (#), type (:-->))

-- | @since 1.0.0
pconst ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s (a :--> b :--> a)
pconst = phoistAcyclic $ plam const

-- | @since 1.0.0
pidentity ::
    forall (a :: S -> Type) (s :: S).
    Term s (a :--> a)
pidentity = phoistAcyclic $ plam id

-- | @since 1.0.0
papply ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s ((a :--> b) :--> a :--> b)
papply = phoistAcyclic $ plam $ \f x -> f # x
