module Plutarch.Extra.Optics (
  HasLabelledGetters,
  inspect,
  inspects,
) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Kind (Constraint)
import GHC.TypeLits (Symbol)
import Optics.Getter (A_Getter, view, views)
import Optics.Label (LabelOptic)
import Optics.Optic (Is, Optic')

{- | Describes that a type @s@ has a collection of labelled optics, all of type
 @k@, which is at least a getter. @labels@ describes which optics @s@ must
 have, as name-result pairs.

 = Note

 This type family unfortunately has two caveats to its use:

 - Redundant constraints resulting from its use won't be picked up by GHC
   warnings.
 - If @labels@ is empty, you will get an overlapping instances error.

 Keep these in mind when using.

 @since 3.10.3
-}
type family HasLabelledGetters (k :: Type) (s :: Type) (labels :: [(Symbol, Type)]) :: Constraint where
  HasLabelledGetters k s '[] = (k `Is` A_Getter)
  HasLabelledGetters k s ('(sym, t) ': labels) =
    (LabelOptic sym k s s t t, HasLabelledGetters k s labels)

{- | 'view' the 'MonadReader' environment using the provided optic.

 @since 3.14.3
-}
{-# INLINEABLE inspect #-}
inspect ::
  forall
    (m :: Type -> Type)
    (r :: Type)
    (k :: Type)
    (is :: [Type])
    (a :: Type).
  (MonadReader r m, Is k A_Getter) =>
  Optic' k is r a ->
  m a
inspect opt = asks (view opt)

{- | As 'inspect', but using 'views' instead of 'view'.

 @since 3.14.3
-}
{-# INLINEABLE inspects #-}
inspects ::
  forall
    (m :: Type -> Type)
    (r :: Type)
    (k :: Type)
    (is :: [Type])
    (a :: Type)
    (b :: Type).
  (MonadReader r m, Is k A_Getter) =>
  Optic' k is r a ->
  (a -> b) ->
  m b
inspects opt f = asks (views opt f)
