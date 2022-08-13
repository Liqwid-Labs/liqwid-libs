{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Benchmark.Plutarch (
  mkTermImplMetaData,
  sampleTerm,
  sampleTerm',
  pbenchAllSizesUniform,
  pbenchNonTinySizesRandomUniform,
  pbenchSizesRandomCached,
  pbenchSizesRandom,
) where

import Control.Monad.Primitive (MonadPrim, PrimMonad, PrimState)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)
import Plutarch.Benchmark.Common (ImplData (..))
import Plutarch.Benchmark.Plutus (
  BudgetExceeded,
  Costs,
  ImplMetaData,
  PlutusCostAxis,
  mkScriptImplMetaData,
  sampleDebuggableScript,
 )
import Plutarch.Benchmark.Sized (
  SSample,
  SUniversalGen,
  benchAllSizesUniform,
  benchNonTinySizesRandomUniform,
  benchSizesRandom,
  benchSizesRandomCached,
 )
import Plutarch.Extra.Compile (mustCompile)
import Plutarch.Extra.DebuggableScript (mustCompileD)
import Plutarch.Extra.Precompile (CompiledTerm, compile', toDebuggableScript)
import Plutarch.Prelude (ClosedTerm, S)
import System.Random (RandomGen)

mkTermImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  ClosedTerm a ->
  ImplMetaData
mkTermImplMetaData name term = mkScriptImplMetaData name $ mustCompile term

sampleTerm :: ClosedTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTerm term = sampleDebuggableScript $ mustCompileD term

sampleTerm' :: CompiledTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTerm' = sampleDebuggableScript . toDebuggableScript

-- | See 'benchAllSizesUniform'.
pbenchAllSizesUniform ::
  forall
    (a :: Type)
    (f :: S -> Type)
    (b :: S -> Type)
    (m :: Type -> Type)
    (s :: Type).
  ( Hashable a
  , PrimMonad m
  , (s ~ PrimState m)
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
pbenchAllSizesUniform
  domainGen
  funName
  pfun
  applyPFun
  desiredSampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchAllSizesUniform
        domainGen
        (sampleTerm' . applyPFun pfun')
        desiredSampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchNonTinySizesRandomUniform'.
pbenchNonTinySizesRandomUniform ::
  forall
    (a :: Type)
    (f :: S -> Type)
    (b :: S -> Type)
    (m :: Type -> Type)
    (s :: Type).
  ( Hashable a
  , MonadPrim s m
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
pbenchNonTinySizesRandomUniform
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchNonTinySizesRandomUniform
        randomGen
        (sampleTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchSizesRandomCached'.
pbenchSizesRandomCached ::
  forall
    (a :: Type)
    (f :: S -> Type)
    (b :: S -> Type)
    (m :: Type -> Type)
    (s :: Type).
  ( Hashable a
  , MonadPrim s m
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
pbenchSizesRandomCached
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchSizesRandomCached
        randomGen
        (sampleTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchSizesRandomCached'.
pbenchSizesRandom ::
  forall
    (a :: Type)
    (f :: S -> Type)
    (b :: S -> Type)
    (m :: Type -> Type)
    (s :: Type).
  ( Hashable a
  , MonadPrim s m
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
        (sampleTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun
