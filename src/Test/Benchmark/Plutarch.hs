module Test.Benchmark.Plutarch (
  mkTermImplMetaData,
  sampleTerm,
  sampleTerm',
  sampleTouchTerm,
  sampleTouchTerm',
  pbenchSizesUniversal,
  pbenchSizesRandom,
  pbenchSizesRandomNonUniform,
) where

import Control.Monad.ST.Class (MonadST)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Plutarch (compile)
import System.Random (RandomGen)
import Test.Benchmark.Common (ImplData (..))
import Test.Benchmark.PTouch (PTouch (ptouch), ptouch')
import Test.Benchmark.Plutus (
  BudgetExceeded,
  Costs,
  ImplMetaData,
  PlutusCostAxis,
  mkScriptImplMetaData,
  sampleScript,
 )
import Test.Benchmark.Precompile (CompiledTerm, compile', toScript, (###~))
import Test.Benchmark.Sized (
  SSample,
  SUniversalGen,
  benchSizesRandom,
  benchSizesRandomNonUniform,
  benchSizesUniversal,
 )

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

sampleTouchTerm ::
  PTouch a =>
  ClosedTerm a ->
  Either (BudgetExceeded PlutusCostAxis) Costs
sampleTouchTerm term = sampleScript $ compile $ ptouch # term

sampleTouchTerm' ::
  PTouch a =>
  CompiledTerm a ->
  Either (BudgetExceeded PlutusCostAxis) Costs
sampleTouchTerm' = sampleScript . toScript . (ptouch' ###~)

-- | See 'benchSizesUniversal'.
pbenchSizesUniversal ::
  forall (a :: Type) (m :: Type -> Type) (f :: S -> Type) (b :: S -> Type).
  ( Eq a
  , Ord a
  , Show a
  , Hashable a
  , MonadST m
  , PTouch b
  ) =>
  -- | Size-dependent input domain generator.
  SUniversalGen a ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | Desired sample size per input size.
  --
  -- The actual sample size will be exactly
  -- @min (cardinalityOfSize inputSize) desiredSampleSizePerInputSize@
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchSizesUniversal
  domainGen
  funName
  pfun
  applyPFun
  desiredSampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchSizesUniversal
        domainGen
        (sampleTouchTerm' . applyPFun pfun')
        desiredSampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchSizesRandom'.
pbenchSizesRandom ::
  forall (a :: Type) (m :: Type -> Type) (f :: S -> Type) (b :: S -> Type).
  ( Eq a
  , Ord a
  , Show a
  , Hashable a
  , MonadST m
  , PTouch b
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchSizesRandom
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchSizesRandom
        randomGen
        (sampleTouchTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchSizesRandomNonUniform'.
pbenchSizesRandomNonUniform ::
  forall (a :: Type) (m :: Type -> Type) (f :: S -> Type) (b :: S -> Type).
  ( Eq a
  , Ord a
  , Show a
  , Hashable a
  , MonadST m
  , PTouch b
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchSizesRandomNonUniform
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchSizesRandomNonUniform
        randomGen
        (sampleTouchTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun