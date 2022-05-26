module Plutarch.Extra.Map (
    plookup,
    plookup',
    pmapFromList,
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
    pcon,
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Builtin (PAsData, PBuiltinList, pfstBuiltin, 
  pfromData, psndBuiltin, PIsData, PBuiltinPair, PBuiltinList (PNil, PCons),
  PBuiltinMap, pdata, ppairDataBuiltin)
import Data.Foldable (foldl')
import Plutarch.Extra.Functor (pfmap)
import Plutarch.Extra.TermCont (pmatchC, pletC)
import Plutarch.Maybe (PMaybe (PNothing, PJust))
import Plutarch.List (pfind)
import Plutarch.Bool (PBool, PEq ((#==)))
import Plutarch.Trace (ptraceError)

-- | @since 1.0.0
plookup :: forall (k :: S -> Type) (v :: S -> Type) (s :: S) . 
  (PIsData v, PIsData k, PEq k) =>
  Term s (k :--> PMap k v :--> PMaybe v)
plookup = phoistAcyclic $ plam $ \x m -> unTermCont $ do
  PMap kvs <- pmatchC m
  res <- pmatchC (pfind # (go # x) # kvs)
  pure $ case res of 
    PNothing -> pcon PNothing
    PJust y -> pcon . PJust . pfromData $ psndBuiltin # y
  where
    go :: forall (s' :: S) . 
      Term s' (k :--> PBuiltinPair (PAsData k) (PAsData v) :--> PBool)
    go = phoistAcyclic $ plam $ \x p -> unTermCont $ do
      k' <- pletC (pfromData $ pfstBuiltin # p)
      pure $ k' #== x

-- | @since 1.0.0
plookup' :: forall (k :: S -> Type) (v :: S -> Type) (s :: S) . 
  (PIsData v, PIsData k, PEq k) => 
  Term s (k :--> PMap k v :--> v)
plookup' = phoistAcyclic $ plam $ \x m -> unTermCont $ do
  res <- pmatchC (plookup # x # m)
  pure $ case res of 
    PNothing -> ptraceError "plookup': Could not find key."
    PJust v -> v

-- | @since 1.0.0
pmapFromList :: forall (k :: S -> Type) (v :: S -> Type) (s :: S) . 
  (PIsData k, PIsData v) =>
  [(Term s k, Term s v)] -> 
  Term s (PMap k v)
pmapFromList = pcon . PMap . foldl' go (pcon PNil)
  where
    go :: 
      Term s (PBuiltinMap k v) -> 
      (Term s k, Term s v) -> 
      Term s (PBuiltinMap k v)
    go acc (k, v) = unTermCont $ do
      k' <- pletC (pdata k)
      v' <- pletC (pdata v)
      p <- pletC (ppairDataBuiltin # k' # v')
      pure . pcon . PCons p $ acc

-- | @since 1.0.0
pkeys ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    Term s (PMap k v :--> PBuiltinList (PAsData k))
pkeys = phoistAcyclic $
    plam $ \m -> unTermCont $ do
        PMap kvs <- pmatchC m
        pure $ pfmap # pfstBuiltin # kvs
