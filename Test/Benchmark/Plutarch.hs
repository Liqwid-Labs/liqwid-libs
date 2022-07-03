module Test.Benchmark.Plutarch (
  mkTermImplMetaData,
  sampleTerm,
) where

import Data.Text (Text)
import Plutarch (compile)
import Test.Benchmark.Plutus (
  BudgetExceeded,
  Costs,
  ImplMetaData,
  mkScriptImplMetaData,
  sampleScript,
 )

mkTermImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  ClosedTerm a ->
  ImplMetaData
mkTermImplMetaData name term = mkScriptImplMetaData name $ compile term

sampleTerm :: ClosedTerm a -> Either BudgetExceeded Costs
sampleTerm term = sampleScript $ compile term