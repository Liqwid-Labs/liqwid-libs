module Plutarch.Extra.Optics (
  HasLabelledGetters,
) where

import Data.Kind (Constraint)
import GHC.TypeLits (Symbol)
import Optics.Getter (A_Getter)
import Optics.Label (LabelOptic)
import Optics.Optic (Is)

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
