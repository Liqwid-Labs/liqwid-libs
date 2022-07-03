{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Benchmarking with a focus on running on many different input sizes and inputs.
module Test.Benchmark.Sized (
  SSample (..),
  DomainSize (..),
  SDomainGen (..),
  mkSDomainPureSTGen,
  mkSDomainSTGen,
  sizedPureRandomGen,
  benchSizes,
) where

import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (MonadST, liftST)
import Control.Monad.Trans (lift)
import Data.HashTable.ST.Basic qualified as HashTable
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Random (RandomGen, mkStdGen)
import System.Random.Stateful (RandomGenM (applyRandomGenM), STGenM, applySTGen, newSTGenM)

-- | Holds sample and metadata for a certain input size
data SSample s = SSample
  { inputSize :: Int
  , coverage :: Maybe Float
  -- ^ Sample size / Number of possible inputs at that size
  , sampleSize :: Int
  -- ^ Sample size.
  , sample :: s
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
  -- Make sure to keep the your random seed state in the monadic context,
  -- see also 'sizedPureRandomGen'.
  }

-- | Make SDomainGen based on STGenM. Random seed originates here.
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
mkSDomainPureSTGen ::
  forall m a.
  ( Monad m
  , MonadST m
  ) =>
  (Int -> DomainSize) ->
  Maybe (Int -> [a]) ->
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  m (SDomainGen m a)
mkSDomainPureSTGen cardinalityOfSize exhaustiveGen pureRandomGen =
  mkSDomainSTGen cardinalityOfSize exhaustiveGen (applySTGen . pureRandomGen)

{- | Adapt a size-parametrized pure random gen for use in 'SSample'.

Solves the problem of keeping the random seed state var (like 'STGenM')
in a monadic context.

Usage with IO:
@
let gen = SDomainGen _ _ (sizedPureRandomGen $ \n -> genWord8)
g <- newIOGenM (mkStdGen 42)
ssamples <- runReaderT (benchSizes gen _sampleFun _inputs _desiredSamplesPerInput) g
print ssamples
@

Usage with ST:
@
let gen = SDomainGen _ _ (sizedPureRandomGen $ \n -> genWord8)
g <- liftST $ newSTGenM (mkStdGen 42)
ssamples <- liftST $ runReaderT (benchSizes gen _sampleFun _inputs _desiredSamplesPerInput) g
print ssamples
@
-}
sizedPureRandomGen ::
  forall (g :: Type) (r :: Type) (m :: Type -> Type) (a :: Type).
  ( RandomGenM g r m
  , MonadST m
  ) =>
  (Int -> r -> (a, r)) ->
  Int ->
  ReaderT g m a
sizedPureRandomGen pureRandomGen size =
  ask >>= lift . applyRandomGenM (pureRandomGen size)

{- | Benchmark for various input sizes. Handles small input sizes correctly.

 Output contains a list of samples for each input size.

 This list should be not be kept in memory, better process
 it into arrays right away, or write to file.
 TODO An actual Stream might be a better choice
-}
benchSizes ::
  forall (a :: Type) (m :: Type -> Type) (s :: Type).
  ( Eq a
  , Ord a
  , Show a -- TODO print input when exception happens
  , Hashable a
  , MonadST m
  ) =>
  -- | Size-dependent input domain generator.
  SDomainGen m a ->
  -- | Sampling function.
  (a -> s) ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  -- | Desired number of samples per input size
  Int ->
  m [SSample [s]]
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
      benchInputSize :: Int -> m (SSample [s])
      benchInputSize inputSize = do
        inputs <- genInputs
        let sample = fmap sampleFun inputs
        pure $ SSample {inputSize, coverage, sampleSize = numInputs, sample}
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