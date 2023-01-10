module Plutarch.Extra.Optics (
  -- * Type families for constraints
  HasLabelled,
  HasLabelledGetters,

  -- * Helper functions
  inspect,
  inspects,
  guarantee,
  guarantees,
) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import GHC.TypeLits (Symbol)
import Optics.AffineFold (An_AffineFold, preview, previews)
import Optics.Getter (A_Getter, view, views)
import Optics.Label (LabelOptic)
import Optics.Optic (Is, Optic')

{- | Describes that a type @s@ has a collection of labelled optics, all of type
 @k@, which is at least as capable as @opt@ (though could be more so).
 @labels@ describes which optics @s@ must have, as name-result pairs.

 = Note

 This type family unfortunately has two caveats to its use:

 - Redundant constraints resulting from its use won't be picked up by GHC
 warnings.
 - If @labels@ is empty, you will get an overlapping instances error.

 Keep these in mind when using.

 @since 3.15.2
-}
type family
  HasLabelled
    (opt :: Type)
    (k :: Type)
    (s :: Type)
    (labels :: [(Symbol, Type)]) ::
    Constraint
  where
  HasLabelled opt k s '[] = (k `Is` opt)
  HasLabelled opt k s ('(sym, t) ': labels) =
    (LabelOptic sym k s s t t, HasLabelled opt k s labels)

{- | Short for @'HasLabelled' 'A_Getter'@.

 @since 3.10.3
 Note from Koz: To avoid breaking everything, I've made this a type synonym
 for now, while folks act on the deprecation warning. Not ideal, but far less
 damaging.
-}
{-# DEPRECATED HasLabelledGetters "Use HasLabelled A_Getter instead." #-}

type HasLabelledGetters (k :: Type) (s :: Type) (labels :: [(Symbol, Type)]) =
  HasLabelled A_Getter k s labels

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

{- | As 'preview', but also gives a default in case the optic \'misses\'.

 @since 3.15.2
-}
{-# INLINEABLE guarantee #-}
guarantee ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (a :: Type).
  (Is k An_AffineFold) =>
  a ->
  Optic' k is s a ->
  s ->
  a
guarantee x opt = fromMaybe x . preview opt

{- | As 'previews', but also gives a default in case the optic \'misses\'.

 @since 3.15.2
-}
{-# INLINEABLE guarantees #-}
guarantees ::
  forall (k :: Type) (is :: [Type]) (s :: Type) (a :: Type) (b :: Type).
  (Is k An_AffineFold) =>
  b ->
  Optic' k is s a ->
  (a -> b) ->
  s ->
  b
guarantees x opt f = fromMaybe x . previews opt f
