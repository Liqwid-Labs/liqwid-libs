module Plutarch.Extra.Function (
    pconst,
    pidentity,
    papply,
    pon,
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

{- | Plutarch level 'Data.Function.on'.
     @since 1.3.0
-}
pon ::
    forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
    Term s ((b :--> b :--> c) :--> (a :--> b) :--> a :--> a :--> c)
pon = phoistAcyclic $
    plam $ \f g x y ->
        let a = g # x
            b = g # y
         in f # a # b
