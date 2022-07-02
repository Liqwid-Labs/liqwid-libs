{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Benchmarking Plutarch code, with a focus on running on many different inputs.
module Test.Benchmark.Sized (
  ImplMetaData (..),
  mkImplMetaData,
  CostAxis (..),
  Cost (..),
  Costs (..),
  sampleScript,
  sampleTerm,
  SSamples (..),
  DomainSize (..),
  SDomainGen (..),
  mkSDomainPureGen,
  mkSDomainSTGen,
  benchSizes,
) where

import Codec.Serialise (serialise)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (MonadST, liftST)
import Data.ByteString.Lazy qualified as LBS
import Data.HashTable.ST.Basic qualified as HashTable
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Plutarch (compile)
import Plutarch.Evaluate (evalScript)
import PlutusCore.Evaluation.Machine.ExBudget (ExRestrictingBudget (ExRestrictingBudget))
import PlutusCore.Evaluation.Machine.Exception (ErrorWithCause (..), EvaluationError (InternalEvaluationError, UserEvaluationError))
import PlutusLedgerApi.V1 (
  ExBudget (ExBudget),
  ExCPU (..),
  ExMemory (..),
  Script,
 )
import System.Random (RandomGen, mkStdGen)
import System.Random.Stateful (STGenM, applySTGen, newSTGenM)
import UntypedPlutusCore.Evaluation.Machine.Cek (CekUserError (CekEvaluationFailure, CekOutOfExError))

data ImplMetaData = ImplMetaData
  { name :: Text
  -- ^ Name of the implementation. Make sure it's unique.
  , scriptSize :: Int
  -- ^ Size of the script without inputs.
  }
  deriving stock (Show, Eq, Ord, Generic)

mkImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  ClosedTerm a ->
  ImplMetaData
mkImplMetaData name term = ImplMetaData {name, scriptSize}
  where
    scriptSize = fromIntegral . LBS.length . serialise $ compile term

data CostAxis = CPU | Mem deriving stock (Show, Eq, Ord, Generic)

-- | Based on Int, since the Plutus budget types are Int internally as well
newtype Cost (a :: CostAxis) = Cost Int deriving stock (Show, Eq, Ord, Generic)

data Costs
  = Costs
      { cpuCost :: Cost 'CPU
      , memCost :: Cost 'Mem
      }
  | BudgetExceeded {exceededAxis :: CostAxis}
  deriving stock (Show, Eq, Ord, Generic)

sampleScript :: Script -> Costs
sampleScript script =
  case res of
    Right _ -> Costs {cpuCost, memCost}
    Left (ErrorWithCause evalErr _) ->
      case evalErr of
        InternalEvaluationError _ -> error "Internal evaluation-error!"
        UserEvaluationError e ->
          case e of
            CekEvaluationFailure -> error "Script failed for non-budget reason!"
            CekOutOfExError (ExRestrictingBudget (ExBudget rcpu rmem)) ->
              if rcpu < 0
                then BudgetExceeded CPU
                else
                  if rmem < 0
                    then BudgetExceeded Mem
                    else
                      error $
                        "Got CekOutOfExError, but ExRestrictingBudget contains "
                          <> "neither negative CPU nor negative Memory!"
  where
    (res, ExBudget (ExCPU rawCpu) (ExMemory rawMem), _traces) = evalScript script
    cpuCost = Cost $ fromIntegral rawCpu
    memCost = Cost $ fromIntegral rawMem

sampleTerm :: ClosedTerm a -> Costs
sampleTerm term = sampleScript $ compile term

-- | Holds samples for a certain input size
data SSamples s = SSamples
  { inputSize :: Int
  , coverage :: Maybe Float
  -- ^ Number of samples / Number of possible inputs at that size
  , numSamples :: Int
  -- ^ Number of contained samples.
  , samples :: [s]
  -- ^ List of samples.
  --
  -- This list should be not be kept in memory, better process
  -- it into arrays right away.
  -- TODO Hopefully it gets garbage-collected while generating lazily.
  -- An actual Stream might be a better choice
  }
  deriving stock (Show, Generic)

data DomainSize = DomainSize Natural | HugeDomainSize deriving stock (Eq, Ord, Show, Generic)

{- | Size-dependent input domain generator.

 Holds everything needed to generate the set of inputs for a given input size.
-}
data SDomainGen (m :: Type -> Type) (a :: Type) = SDomainGen
  { cardinalityOfSize :: Int -> DomainSize
  -- ^ Number of possible inputs of a given input size
  --
  -- 'HugeDomainSize' is interpreted as "trust me, it's more than the desired
  -- number of samples per size". If you are wrong, that might cause
  -- non-termination.
  , exhaustiveGen :: Maybe (Int -> [a])
  -- ^ Exhaustive input generator, given the input size
  , randomGen :: Int -> m a
  -- ^ Random input generator, given the input size.
  -- You can use 'Test.QuickCheck.generate :: Gen a -> IO a' here.
  }

-- | Make SDomainGen using a STGen. Random seed originates here.
mkSDomainSTGen ::
  forall m a.
  ( Monad m
  , MonadST m
  ) =>
  (Int -> DomainSize) ->
  Maybe (Int -> [a]) ->
  (forall (g :: Type) (s :: Type). RandomGen g => Int -> STGenM g s -> ST s a) ->
  m (SDomainGen m a)
mkSDomainSTGen cardinalityOfSize exhaustiveGen stRandomGen = do
  g <- liftST $ newSTGenM (mkStdGen 42)
  let randomGen inputSize = liftST $ stRandomGen inputSize g
  pure $ SDomainGen {cardinalityOfSize, exhaustiveGen, randomGen}

-- | Make SDomainGen using a pure random gen. Random seed originates here.
mkSDomainPureGen ::
  forall m a.
  ( Monad m
  , MonadST m
  ) =>
  (Int -> DomainSize) ->
  Maybe (Int -> [a]) ->
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  m (SDomainGen m a)
mkSDomainPureGen cardinalityOfSize exhaustiveGen pureRandomGen =
  mkSDomainSTGen cardinalityOfSize exhaustiveGen (applySTGen . pureRandomGen)

-- | Benchmark for various input sizes. Handles small input sizes correctly.
benchSizes ::
  forall (a :: Type) (m :: Type -> Type) (sample :: Type).
  ( Eq a
  , Ord a
  , Show a -- TODO print input when exception happens
  , Hashable a
  , Monad m
  , MonadST m
  ) =>
  -- | Size-dependent input domain generator.
  SDomainGen m a ->
  -- | Sampling function.
  (a -> sample) ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  -- | Desired number of samples per input size
  Int ->
  m [SSamples sample]
benchSizes
  SDomainGen
    { cardinalityOfSize
    , randomGen
    , exhaustiveGen
    }
  sampleFun
  sizes
  desiredSamplesPerInput =
    mapM benchInputSize sizes
    where
      benchInputSize :: Int -> m (SSamples sample)
      benchInputSize inputSize = do
        inputs <- genInputs
        let samples = fmap sampleFun inputs
        pure $ SSamples {inputSize, coverage, numSamples = numInputs, samples}
        where
          -- TODO for high coverage significantly above 50%, should probably generate exhaustively and drop some inputs
          coverage = case cardinality of
            HugeDomainSize -> Nothing
            DomainSize card -> Just $ fromIntegral numInputs / fromIntegral card
          cardinality = cardinalityOfSize inputSize
          (numInputs, genInputs) = case cardinality of
            HugeDomainSize -> (desiredSamplesPerInput, genRandom)
            DomainSize card ->
              if card <= fromIntegral desiredSamplesPerInput
                then case exhaustiveGen of
                  Just genExhaustive -> (inputSize, pure $ genExhaustive inputSize)
                  Nothing -> (fromIntegral card, genRandom)
                else (desiredSamplesPerInput, genRandom)
          genRandom :: m [a]
          genRandom = do
            ht <- liftST $ HashTable.newSized inputSize
            let loop 0 = pure []
                loop n = do
                  input <- randomGen inputSize
                  isNew <-
                    -- just abusing this mutable hashtable as a set
                    -- lookup and insert at the same time
                    liftST $
                      HashTable.mutate ht input $
                        maybe (Just (), True) (\() -> (Just (), False))
                  if isNew
                    then (input :) <$> loop (n - 1)
                    else loop n
            loop numInputs