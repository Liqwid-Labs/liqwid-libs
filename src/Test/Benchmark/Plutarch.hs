module Test.Benchmark.Plutarch (
  mkTermImplMetaData,
  sampleTerm,
  sampleTerm',
  sampleTouchTerm,
  sampleTouchTerm',
) where

import Data.Text (Text)
import Plutarch (compile)
import Test.Benchmark.Plutus (
  BudgetExceeded,
  Costs,
  ImplMetaData,
  PlutusCostAxis,
  mkScriptImplMetaData,
  sampleScript,
 )
import Test.Benchmark.Precompile (CompiledTerm, toScript, (###~))
import Test.Benchmark.PTouch (PTouch (ptouch), ptouch')

mkTermImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  ClosedTerm a ->
  ImplMetaData
mkTermImplMetaData name term = mkScriptImplMetaData name $ compile term

sampleTerm :: ClosedTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTerm term = sampleScript $ compile term

sampleTerm' :: CompiledTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTerm' = sampleScript . toScript

sampleTouchTerm :: PTouch a => ClosedTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTouchTerm term = sampleScript $ compile $ ptouch # term

sampleTouchTerm' :: PTouch a => CompiledTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTouchTerm' = sampleScript . toScript . (ptouch' ###~)