{- | Workaround for accidental lazyness in Scott-encoded results of functions being benchmarked.

This won't be needed in Plutarch 1.2.
-}
module Test.Benchmark.PTouch (PTouch(ptouch), ptouch', touching) where

import Test.Benchmark.Precompile (CompiledTerm, compile')

ptouch' ::
  forall (a :: S -> Type).
  PTouch a =>
  CompiledTerm (a :--> PUnit)
ptouch' = compile' ptouch

class PTouch (a :: S -> Type) where
  {- | Forces full evaluation of Scott-encoded lists.

  Beware: The evaluation results do NOT persist. This only results in paying the
  cost for full evaluation.
  -}
  ptouch :: forall (s :: S). Term s (a :--> PUnit)
  ptouch = plam $ \_ -> pcon PUnit

------------- Instances for builtin types that can't be pmatched on

deriving anyclass instance PTouch PInteger
deriving anyclass instance PTouch PString
deriving anyclass instance PTouch PByteString

------------- Instance helpers

touching ::
  forall (s :: S) (a :: S -> Type) (b :: S -> Type).
  PTouch a =>
  Term s a ->
  Term s b ->
  Term s b
touching a b = plet (ptouch # a) $ const b

------------- Instances

instance PTouch a => PTouch (PList a) where
  ptouch = phoistAcyclic $
    pfix #$ plam $ \self list ->
      pmatch list $ \case
        PSNil -> pcon PUnit
        PSCons x xs -> touching x $ self # xs
