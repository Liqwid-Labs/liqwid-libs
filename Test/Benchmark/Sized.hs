{-# LANGUAGE NoFieldSelectors #-}

-- | Benchmarking with a focus on running on many different input sizes and inputs.
module Test.Benchmark.Sized (
  SSample (..),
  DomainCardinality (..),
  SDomainGen (..),
  mkSDomainPureSTGen,
  mkSDomainSTGen,
  sizedPureRandomGen,
  benchSizes,
) where

import Control.Monad (filterM)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (MonadST, liftST)
import Control.Monad.Trans (lift)
import Data.HashTable.ST.Basic qualified as HashTable
import Data.Hashable (Hashable)
import Data.Maybe (isNothing)
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
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

data DomainCardinality
  = DomainCardinality Natural
  | HugeDomainCardinality
  deriving stock (Eq, Ord, Show, Generic)

{- | Size-dependent input domain generator.

 Holds everything needed to generate the set of inputs for a given input size.
-}
data SDomainGen (m :: Type -> Type) (a :: Type) = SDomainGen
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
  , randomGen :: Int -> m a
  -- ^ Random input generator, given the input size.
  --
  -- You can use 'Test.QuickCheck.generate :: Gen a -> IO a' here.
  -- You need to keep the your random seed state in the monadic context,
  -- see also 'sizedPureRandomGen'.
  , indexGen :: Int -> m Int
  -- ^ A way to generate random indices in @m@, from 0 to <arg> inclusive.
  --
  -- When @desiredSampleSize / cardinality > 0.5@, 'exhaustiveGen' is used.
  -- 'indexGen' gets used to generate indices to drop from 'exhaustiveGen' output.
  }

-- | Make SDomainGen based on STGenM. Random seed originates here.
mkSDomainSTGen ::
  forall m a.
  ( Monad m
  , MonadST m
  ) =>
  (Int -> DomainCardinality) ->
  (Int -> [a]) ->
  (forall (g :: Type) (s :: Type). RandomGen g => Int -> STGenM g s -> ST s a) ->
  (Int -> m Int) ->
  m (SDomainGen m a)
mkSDomainSTGen cardinalityOfSize exhaustiveGen stRandomGen indexGen = do
  g <- liftST $ newSTGenM (mkStdGen 42)
  let randomGen inputSize = liftST $ stRandomGen inputSize g
  pure $ SDomainGen {cardinalityOfSize, exhaustiveGen, randomGen, indexGen}

-- | Make SDomainGen using a pure random gen. Random seed originates here.
mkSDomainPureSTGen ::
  forall m a.
  ( Monad m
  , MonadST m
  ) =>
  (Int -> DomainCardinality) ->
  (Int -> [a]) ->
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  (Int -> m Int) ->
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

TODO Could use 'MonadRandom', but that is not exception-safe. Not sure if
  exception-safety will be needed.
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

-- TODO Should probably just go with a MonadRandom and write an adapter for QuickCheck stuff
-- that works like in
-- https://hackage.haskell.org/package/hedgehog-quickcheck-0.1.1/docs/src/Hedgehog.Gen.QuickCheck.html
-- -> Would have known way to save the seed before a crash, or to continue after ctrl+c

{- | Benchmark for various input sizes. Handles small input sizes correctly.

 Output contains a list of samples for each input size.

 The list of sample elements '[s]' should be not be kept in memory, better
 process it into arrays right away, or write to file.
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
  -- | Desired number of samples per input size.
  --
  -- The actual number of samples will be exactly
  -- @min (cardinalityOfSize inputSize) desiredSamplesPerInputSize@
  Int ->
  m [SSample [s]]
benchSizes
  SDomainGen
    { cardinalityOfSize
    , randomGen
    , exhaustiveGen
    , indexGen
    }
  sampleFun
  sizes
  desiredSamplesPerInputSize =
    mapM benchInputSize sizes
    where
      benchInputSize :: Int -> m (SSample [s])
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
                  ( inputSize
                  , pure $ verifyCard card $ exhaustiveGen inputSize
                  )
                else
                  ( desiredSamplesPerInputSize
                  , if coverage > Just 0.5
                      then genRandomSubset card
                      else genRandom
                  )
          -- coverage close to 1 would have extreme slowdown if we didn't do this
          genRandomSubset :: Natural -> m [a]
          genRandomSubset card = do
            let dropNum = fromIntegral card - numInputs
            -- fill a hashtable with dropNum indices into exhaustiveGen output
            ht <- liftST $ HashTable.newSized inputSize
            let loop (-1) = pure ()
                loop n = do
                  input <- indexGen (fromIntegral card - 1)
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