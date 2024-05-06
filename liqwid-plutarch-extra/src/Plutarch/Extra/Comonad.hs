module Plutarch.Extra.Comonad (
  PExtend (..),
  PComonad (..),
) where

import Plutarch.Extra.Functor (PFunctor (PSubcategory))
import Plutarch.List (puncons)

-- | @since 1.0.0
class (PFunctor w) => PExtend (w :: (S -> Type) -> S -> Type) where
  pextend ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    (PSubcategory w a, PSubcategory w b) =>
    Term s ((w a :--> b) :--> w a :--> w b)

{- | Applies the given function over every proper suffix of a 'PList', from
 longest to shortest, and returns their results in a 'PList'.

 @since 1.0.0
-}
instance PExtend PList where
  pextend ::
    forall (a :: S -> Type) (b :: S -> Type) (s :: S).
    Term s ((PList a :--> b) :--> PList a :--> PList b)
  pextend = phoistAcyclic $
    plam $ \f xs -> unTermCont $ do
      t <- pmatchC (puncons # xs)
      case t of
        PNothing -> pure pnil
        PJust t' -> do
          PPair _ ttail <- pmatchC t'
          pure $ go # f # ttail
    where
      go ::
        forall (s' :: S).
        Term s' ((PList a :--> b) :--> PList a :--> PList b)
      go = phoistAcyclic $
        plam $ \f xs -> unTermCont $ do
          res <- pletC (f # xs)
          t <- pmatchC (puncons # xs)
          case t of
            PNothing -> pure $ pcons # res # pnil
            PJust t' -> do
              PPair _ ttail <- pmatchC t'
              pure $ pcons # res # (go # f # ttail)

-- | @since 1.0.0
instance PExtend (PPair a) where
  pextend = phoistAcyclic $
    plam $ \f p -> unTermCont $ do
      PPair x _ <- pmatchC p
      pure . pcon . PPair x $ f # p

-- | @since 1.0.0
class (PExtend w) => PComonad (w :: (S -> Type) -> S -> Type) where
  pextract ::
    forall (a :: S -> Type) (s :: S).
    (PSubcategory w a) =>
    Term s (w a :--> a)

-- | @since 1.0.0
instance PComonad (PPair a) where
  pextract = phoistAcyclic $
    plam $ \p -> unTermCont $ do
      PPair _ t <- pmatchC p
      pure t
