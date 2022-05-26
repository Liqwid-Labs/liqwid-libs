module Plutarch.Extra.Map (
    pkeys,
) where

import Data.Kind (Type)
import Plutarch (
    S,
    Term,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Builtin (PAsData, PBuiltinList, pfstBuiltin)
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.TermCont (pmatchC)

-- | @since 1.0.0
pkeys ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    Term s (PMap k v :--> PBuiltinList (PAsData k))
pkeys = phoistAcyclic $
    plam $ \m -> unTermCont $ do
        PMap kvs <- pmatchC m
        pure $ pfmap # pfstBuiltin # kvs
