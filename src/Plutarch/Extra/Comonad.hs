module Plutarch.Extra.Comonad (
    PComonad (..),
) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Extra.Functor (PFunctor (PSubcategory))
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.Pair (PPair (PPair))

-- | @since 1.0.0
class (PFunctor w) => PComonad (w :: (S -> Type) -> S -> Type) where
    pextract ::
        forall (a :: S -> Type) (s :: S).
        (PSubcategory w a) =>
        Term s (w a :--> a)
    pextend ::
        forall (a :: S -> Type) (b :: S -> Type) (s :: S).
        (PSubcategory w a, PSubcategory w b) =>
        Term s ((w a :--> b) :--> w a :--> w b)

instance PComonad (PPair a) where
    pextract = phoistAcyclic $
        plam $ \p -> unTermCont $ do
            PPair _ t <- pmatchC p
            pure t
    pextend = phoistAcyclic $
        plam $ \f p -> unTermCont $ do
            PPair x _ <- pmatchC p
            pure . pcon . PPair x $ f # p
