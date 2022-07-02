{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Benchmarking with a focus on running on many different input sizes and inputs.
module Test.Benchmark.Sized (
  SSamples (..),
  DomainSize (..),
  SDomainGen (..),
  mkSDomainPureGen,
  mkSDomainSTGen,
  benchSizes,
) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Class (MonadST, liftST)
import Data.HashTable.ST.Basic qualified as HashTable
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Random (RandomGen, mkStdGen)
import System.Random.Stateful (STGenM, applySTGen, newSTGenM)

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