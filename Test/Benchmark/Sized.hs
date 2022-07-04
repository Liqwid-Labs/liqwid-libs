{-# LANGUAGE NoFieldSelectors #-}

-- | Benchmarking with a focus on running on many different input sizes and inputs.
module Test.Benchmark.Sized (
  SSample (..),
  DomainCardinality (..),
  SDomainGen (..),
  benchSizes,
) where

import Control.Monad (filterM)
import Control.Monad.ST.Class (MonadST, liftST)
import Control.Monad.State.Strict (StateT)
import Data.HashTable.ST.Basic qualified as HashTable
import Data.Hashable (Hashable)
import Data.Maybe (isNothing)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Random (RandomGen, StdGen, mkStdGen)
import System.Random.Stateful (StateGenM (StateGenM), applyRandomGenM, runStateGenT_, uniformRM)

-- | Holds sample and metadata for a certain input size
data SSample s = SSample
  { inputSize :: Int
  , coverage :: Maybe Float
  -- ^ Sample size / Number of possible inputs at that size
  , sampleSize :: Int
  -- ^ Sample size.
  , sample :: s
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

data DomainCardinality
  = DomainCardinality Natural
  | HugeDomainCardinality
  deriving stock (Eq, Ord, Show, Generic)

{- | Size-dependent input domain generator.

 Holds everything needed to generate the set of inputs for a given input size.
-}
data SDomainGen (a :: Type) = SDomainGen
  { cardinalityOfSize :: Int -> DomainCardinality
  -- ^ Number of possible inputs of a given input size
  --
  -- This is used to decide between the two generators below, depending
  -- on the desired sample size.
  --
  -- 'HugeDomainCardinality' is interpreted as "trust me, it's more than the
  -- desired sample size at this input size". If you are wrong, that might cause
  -- non-termination.
  , exhaustiveGen :: Int -> [a]
  -- ^ Exhaustive input generator, given the input size
  , randomGen :: forall (g :: Type). RandomGen g => Int -> g -> (a, g)
  -- ^ Random input generator, given the input size.
  --
  -- For a comfortable interface, use something like 'System.Random.Stateful.runStateGen'
  -- or the MonadRandom package.
  --
  -- See 'Test.Benchmark.QuickCheck' for how to use QuickCheck generators here.
  -- You probably want either an uniform distribution, or a distribution that
  -- represents real-world inputs. Keep that in mind when reusing QuickCheck
  -- generators, they might not have been written with that in mind.
  }

{- | Benchmark for various input sizes. Handles small input sizes correctly.

 Output contains a list of samples for each input size.

 The list of sample elements '[s]' should be not be kept in memory, better
 process it into arrays right away, or write to file.
 TODO An actual Stream might be a better choice
-}
benchSizes ::
  forall (a :: Type) (m :: Type -> Type) (se :: Type).
  ( Eq a
  , Ord a
  , Show a -- TODO print input when exception happens
  , Hashable a
  -- TODO could hide that ST is being used, but need effects anyway for displaying progress later
  --   so at least a Monad constraint will probably be involved
  , MonadST m
  ) =>
  -- | Size-dependent input domain generator.
  SDomainGen a ->
  -- | Sampling function: From input to sample element.
  (a -> se) ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  -- | Desired number of samples per input size.
  --
  -- The actual number of samples will be exactly
  -- @min (cardinalityOfSize inputSize) desiredSamplesPerInputSize@
  Int ->
  m [SSample [se]]
benchSizes
  SDomainGen
    { cardinalityOfSize
    , randomGen
    , exhaustiveGen
    }
  sampleFun
  sizes
  desiredSamplesPerInputSize =
    -- using StateGenM to be able to freeze the seed. MonadRandom can't do this..
    runStateGenT_ (mkStdGen 42) $ const $ mapM benchInputSize sizes
    where
      benchInputSize :: Int -> StateT StdGen m (SSample [se])
      benchInputSize inputSize = do
        inputs <- genInputs
        let sample = fmap sampleFun inputs
        pure $ SSample {inputSize, coverage, sampleSize = numInputs, sample}
        where
          coverage = case cardinality of
            HugeDomainCardinality -> Nothing
            DomainCardinality card ->
              Just $ fromIntegral numInputs / fromIntegral card
          cardinality = cardinalityOfSize inputSize
          (numInputs, genInputs) = case cardinality of
            HugeDomainCardinality -> (desiredSamplesPerInputSize, genRandom)
            DomainCardinality card ->
              if card <= fromIntegral desiredSamplesPerInputSize
                then
                  ( fromIntegral card
                  , pure $ verifyCard card $ exhaustiveGen inputSize
                  )
                else
                  ( desiredSamplesPerInputSize
                  , if coverage > Just 0.5
                      then genRandomSubset card
                      else genRandom
                  )
          -- coverage close to 1 would have extreme slowdown if we didn't do this
          genRandomSubset :: Natural -> StateT StdGen m [a]
          genRandomSubset card = do
            let dropNum = fromIntegral card - numInputs
            -- fill a hashtable with dropNum indices into exhaustiveGen output
            ht <- liftST $ HashTable.newSized inputSize
            let loop (-1) = pure ()
                loop n = do
                  input :: Int <- uniformRM (0, fromIntegral card - 1) StateGenM
                  isNew <-
                    -- just abusing this mutable hashtable as a set
                    -- lookup and insert at the same time
                    liftST $
                      HashTable.mutate ht input $
                        maybe (Just (), True) (\() -> (Just (), False))
                  if isNew
                    then loop (n - 1)
                    else loop n
            loop dropNum
            let es = zip [0 ..] (verifyCard card $ exhaustiveGen inputSize)
            es' <- flip filterM es $ \(ix, _) ->
              liftST $ isNothing <$> HashTable.lookup ht ix
            pure $ snd <$> es'
          genRandom :: StateT StdGen m [a]
          genRandom = do
            ht <- liftST $ HashTable.newSized inputSize
            let loop 0 = pure []
                loop n = do
                  input <- applyRandomGenM (randomGen inputSize) StateGenM
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
          verifyCard :: Natural -> [a] -> [a]
          verifyCard card = go (fromIntegral card :: Integer)
            where
              go n (x : xs) = x : go (n - 1) xs
              go n [] =
                if n == 0
                  then []
                  else
                    error $
                      "cardinalityOfSize on inputSize "
                        <> show inputSize
                        <> " was "
                        <> show card
                        <> ", this is off by "
                        <> show n