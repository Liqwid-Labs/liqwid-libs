{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Benchmarking with a focus on running on many different input sizes and
 inputs.

 Read on below to find out what function you want to use.

 Definition: The cardinality of a certain input size refers to the number of
 possible inputs at that size, regarding the method of input generation that is
 being used. An input generator does not necessarily generate the whole domain
 of the function being benchmarked, so the cardinality of the input size can be
 different from the cardinality of the subset of the domain with that size.

 When randomly generating inputs for small input sizes, achieving 100% coverage
 is difficult. Imagine having already generated 9999 inputs of 10000 possible inputs.
 The random generator will have a chance of 1/10000 of hitting the last possible
 input with each attempt, requiring a huge number of attempts.

 'benchAllSizesUniform' exists for this reason, it makes use of exhaustive
 generation for small input sizes and switches over to random generation for
 larger input sizes seamlessly. The price you pay for that is having to write
 both generators, bundled up in a 'SUniversalGen'. Ideally also the cardinality
 function, this makes things more efficient but is not required.

 All other @bench..@ functions in this module make use of random generation only.

 If you want to bench small input sizes without having to write an 'SUniversalGen',
 you should use 'benchSizesRandom'.

 'benchSizesRandomCached' caches results to speed up benching non-uniform random
 distributions, but is currently not multi-threaded. Unlikely to be useful in
 its current form.

 'benchAllSizesUniform' and 'benchNonTinySizesUniform' deduplicate the inputs,
 making them only suitable for uniform distributions when randomly generating.

 'benchSizesRandom' does no deduplication and no result caching. It is suitable
 for any distribution.
-}
module Plutarch.Benchmark.Sized (
  SSample (..),
  Cardinality (..),
  SUniversalGen (..),
  benchAllSizesUniform,
  benchNonTinySizesRandomUniform,
  benchSizesRandomCached,
  benchSizesRandom,
) where

import Control.Monad (filterM, forM, replicateM)
import Control.Monad.Primitive (MonadPrim, stToPrim)
import Control.Monad.State.Strict (StateT)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Csv (
  DefaultOrdered (headerOrder),
  ToNamedRecord,
  header,
  namedRecord,
  toNamedRecord,
  (.=),
 )
import qualified Data.HashTable.ST.Basic as HashTable
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isNothing)
import Data.Primitive.MutVar (newMutVar, readMutVar, writeMutVar)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Optics.TH (makeFieldLabelsNoPrefix)
import System.Random (RandomGen, StdGen, mkStdGen)
import System.Random.Stateful (
  StateGenM (StateGenM),
  applyRandomGenM,
  runStateGenT_,
  uniformRM,
 )
import Text.Printf (printf)

-- | Holds sample and metadata for a certain input size
data SSample s = SSample
  { inputSize :: Int
  , coverage :: Maybe Float
  -- ^ Sample size / Number of possible inputs at that size
  , sampleSize :: Int
  -- ^ Sample size.
  , sample :: s
  }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

makeFieldLabelsNoPrefix ''SSample

instance ToNamedRecord (SSample ()) where
  toNamedRecord (SSample {..}) =
    namedRecord
      [ "input size" .= inputSize
      , "coverage"
          .= (maybe "" (\c -> printf "%.f%%" (c * 100)) coverage :: String)
      , "sample size" .= sampleSize
      ]

instance DefaultOrdered (SSample ()) where
  headerOrder _ =
    header
      [ "input size"
      , "coverage"
      , "sample size"
      ]

data Cardinality
  = Cardinality {exact :: Natural}
  | HugeCardinality
  deriving stock (Eq, Ord, Show, Generic)

makeFieldLabelsNoPrefix ''Cardinality

countCardinalityUpTo :: Int -> [a] -> Cardinality
countCardinalityUpTo lim xs = go xs 0
  where
    go [] c = Cardinality $ fromIntegral c
    go (_ : xs) c =
      if c >= lim
        then HugeCardinality
        else go xs (c + 1)

{- | Universal size-dependent input generator.

 "Universal" relating to small and large input sizes.

 Holds everything needed to generate the set of inputs for a given input size.
 Does not necessarily generate the whole function domain for the size, could
 also focus on certain subsets.

 This is intended for uniform distributions only. The 'exhaustiveGen' is assumed
 to generate each possible input value only once.
-}
data SUniversalGen (a :: Type) = SUniversalGen
  { cardinalityOfSize :: Maybe (Int -> Cardinality)
  -- ^ Number of possible inputs of a given input size
  --
  -- This is used to decide between the two generators below, depending
  -- on the desired sample size.
  --
  -- If this is not given, the 'exhaustiveGen' output is counted up to 130% of
  -- the desired sample size, at which point the count gives up and assumes
  -- 'HugeCardinality'.
  --
  -- 'HugeCardinality' is interpreted as "trust me, it's more than the desired
  -- sample size at this input size". If you are wrong, that might cause
  -- non-termination.
  , exhaustiveGen :: Int -> [a]
  -- ^ Exhaustive input generator, given the input size
  , randomGen :: forall (g :: Type). RandomGen g => Int -> g -> (a, g)
  -- ^ Random input generator, given the input size.
  --
  -- For a comfortable interface, use something like
  -- 'System.Random.Stateful.runStateGen' or the MonadRandom package.
  --
  -- See 'Test.Benchmark.QuickCheck' for how to use QuickCheck generators here.
  -- You probably want either an uniform distribution, or a distribution that
  -- represents real-world inputs. Keep that in mind when reusing QuickCheck
  -- generators, they might not have been written with that in mind.
  }

makeFieldLabelsNoPrefix ''SUniversalGen

{- | Benchmarks for uniformly distributed inputs of any size.

 Uses exhaustive generation on small inputs. This enables to achieve 100%
 coverage on small-ish input sizes that are not tiny. If you don't want to
 write an exhaustive generator, use 'benchSizesRandomCached'

 Deduplicates the randomly generated inputs, so only suitable for uniform
 distributions.

 Output contains a list of samples for each input size.

 The list of sample elements '[s]' should be not be kept in memory, better
 process it into arrays right away, or write to file.
 TODO An actual Stream might be a better choice
-}
benchAllSizesUniform ::
  forall (a :: Type) (se :: Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , -- TODO could hide that ST is being used, but need effects anyway for
    -- displaying progress later so at least a Monad constraint will probably be
    -- involved
    MonadPrim s m
  , NFData se
  ) =>
  -- | Size-dependent input domain generator.
  SUniversalGen a ->
  -- | Sampling function: From input to sample element (a "measurement").
  (a -> se) ->
  -- | Desired sample size per input size.
  --
  -- The actual sample size will be exactly
  -- @min (cardinalityOfSize inputSize) desiredSampleSizePerInputSize@
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m [SSample [se]]
{- TODOs / nice-to-haves
 - progress reporting
 - concurrency
 - maybe streaming
 - dumping the input / seed on crash
 - continuing work after ctrl+c (maybe hash the scripts to check if saved work is obsolete)
 - print input when exception happens
-}
benchAllSizesUniform
  domainGen
  sampleFun
  desiredSampleSizePerInputSize
  sizes = do
    prevCardRef <- newMutVar (Cardinality 0)
    mapM
      ( \inputSize -> do
          prevCard <- readMutVar prevCardRef
          -- using StateGenM to be able to freeze the seed. MonadRandom can't do this..
          (card, ssample) <-
            runStateGenT_ (mkStdGen 42) . const $
              benchInputSizeUniversal
                prevCard
                domainGen
                sampleFun
                desiredSampleSizePerInputSize
                inputSize
          writeMutVar prevCardRef card
          pure ssample
      )
      sizes

benchInputSizeUniversal ::
  forall (a :: Type) (se :: Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , MonadPrim s m
  , NFData se
  ) =>
  -- | Previous 'Cardinality'
  Cardinality ->
  -- | Size-dependent input domain generator.
  SUniversalGen a ->
  -- | Sampling function: From input to sample element (a "measurement").
  (a -> se) ->
  -- | Desired sample size per input size.
  --
  -- The actual sample size will be exactly
  -- @min (cardinalityOfSize inputSize) desiredSampleSizePerInputSize@
  Int ->
  -- | The input size to benchmark with.
  Int ->
  StateT StdGen m (Cardinality, SSample [se])
benchInputSizeUniversal
  prevCard
  SUniversalGen
    { cardinalityOfSize
    , randomGen
    , exhaustiveGen
    }
  sampleFun
  desiredSampleSizePerInputSize
  inputSize = do
    inputs <- genInputs
    let sample = parMap rdeepseq sampleFun inputs
    pure (rememberedCardinality, SSample {inputSize, coverage, sampleSize, sample})
    where
      coverage = case cardinality of
        HugeCardinality -> Nothing
        Cardinality card ->
          Just $ fromIntegral sampleSize / fromIntegral card
      cardinalityOfSize' =
        fromMaybe
          (countCardinalityUpTo ((desiredSampleSizePerInputSize `div` 10) * 13) . exhaustiveGen)
          cardinalityOfSize
      cardinality =
        if prevCard == HugeCardinality
          then HugeCardinality
          else cardinalityOfSize' inputSize
      rememberedCardinality =
        case coverage of
          Nothing -> HugeCardinality
          Just x -> if x < 0.01 then HugeCardinality else cardinality
      (sampleSize, genInputs) = case cardinality of
        HugeCardinality ->
          ( desiredSampleSizePerInputSize
          , genRandomDedup sampleSize (randomGen inputSize)
          )
        Cardinality card ->
          if card <= fromIntegral desiredSampleSizePerInputSize
            then
              ( fromIntegral card
              , pure $ verifyCard card inputSize $ exhaustiveGen inputSize
              )
            else
              ( desiredSampleSizePerInputSize
              , if coverage > Just 0.5
                  then -- coverage close to 1 would have extreme slowdown if we
                  -- didn't do this (shoutout to fourmolu for this horrible
                  -- formatting)

                    genRandomSubset
                      card
                      sampleSize
                      (verifyCard card inputSize $ exhaustiveGen inputSize)
                  else genRandomDedup sampleSize (randomGen inputSize)
              )

verifyCard :: Natural -> Int -> [a] -> [a]
verifyCard card inputSize = go (fromIntegral card :: Integer)
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

{- | Benchmark non-tiny input sizes using deduplicated random inputs only.

 WARNING: Not suitable for very small input sizes, because it will not give up
 until sampleSize distinct inputs have been found! Beware of non-termination on
 input sizes that have a cardinality smaller than the requested sample size. For
 small input sizes, use either 'benchAllSizesUniform' or
 'benchSizesRandomCached'.

 The deduplication makes this only suitable for uniform distributions.

 Output contains a list of samples for each input size.

 The list of sample elements '[s]' should be not be kept in memory, better
 process it into arrays right away, or write to file.
-}
benchNonTinySizesRandomUniform ::
  forall (a :: Type) (se :: Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , -- TODO could hide that ST is being used, but need effects anyway for displaying progress later
    --   so at least a Monad constraint will probably be involved
    MonadPrim s m
  , NFData se
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Sampling function: From input to sample element (a "measurement").
  (a -> se) ->
  -- | The exact sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m [SSample [se]]
-- TODO print input when exception happens
benchNonTinySizesRandomUniform
  randomGen
  sampleFun
  sampleSizePerInputSize
  sizes =
    -- using StateGenM to be able to freeze the seed. MonadRandom can't do this..
    runStateGenT_ (mkStdGen 42) . const $
      mapM (benchInputSize sampleSizePerInputSize) sizes
    where
      benchInputSize sampleSize inputSize = do
        inputs <- genRandomDedup sampleSize (randomGen inputSize)
        let sample = parMap rdeepseq sampleFun inputs
        pure $ SSample {inputSize, coverage = Nothing, sampleSize, sample}

{- | Benchmark input sizes using random inputs only, caching results.

 Warning: This is currently not multi-threaded, use 'benchSizesRandom' instead.

 This does not deduplicate the inputs. Instead, it caches results for inputs to
 increase performance. This makes it suitable for two purposes:

 - Benchmarking non-uniform distributions that mirror the distribution of
   real-world inputs.
 - Benchmarking small input sizes using any distribution, without having to
   implement an exhaustive generator to be able to use 'benchAllSizesUniform'.
   Naturally, only 'benchAllSizesUniform' will be able to achieve 100% coverage
   on small-ish input sizes that are not tiny.

 Output contains a list of samples for each input size.

 The list of sample elements '[s]' should be not be kept in memory, better
 process it into arrays right away, or write to file.

 TODO: This is single-threaded for now, because of accessing the HashTable.

 Threading this seems to be non-trivial because of the HashTable.
 It needs worker threads and channels, with one thread doing the HT
 updates as well as lookups. Might need to defer updates until all
 worker threads are "fed".
-}
benchSizesRandomCached ::
  forall (a :: Type) (se :: Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , MonadPrim s m
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Sampling function: From input to sample element (a "measurement").
  (a -> se) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m [SSample [se]]
benchSizesRandomCached
  randomGen
  sampleFun
  sampleSizePerInputSize
  sizes = do
    runStateGenT_ (mkStdGen 42) . const $
      mapM (benchInputSize sampleSizePerInputSize) sizes
    where
      benchInputSize sampleSize inputSize = do
        ht <- stToPrim $ HashTable.newSized sampleSize
        inputs <-
          replicateM
            sampleSize
            (applyRandomGenM (randomGen inputSize) StateGenM)
        sample <- forM inputs $ \input -> do
          stToPrim $
            HashTable.mutate ht input $ \case
              Nothing -> let se = sampleFun input in (Just se, se)
              jse@(Just se) -> (jse, se)
        pure $ SSample {inputSize, coverage = Nothing, sampleSize, sample}

{- | Benchmark input sizes using random inputs only.

 The results are not cached, allowing this to be multi-threaded.

 This does not deduplicate the inputs.
 This makes it suitable for two purposes:

 - Benchmarking non-uniform distributions that mirror the distribution of
   real-world inputs.
 - Benchmarking small input sizes using any distribution, without having to
   implement an exhaustive generator to be able to use 'benchAllSizesUniform'.
   Naturally, only 'benchAllSizesUniform' will be able to achieve 100% coverage
   on small-ish input sizes that are not tiny.

 Output contains a list of samples for each input size.

 The list of sample elements '[s]' should be not be kept in memory, better
 process it into arrays right away, or write to file.
-}
benchSizesRandom ::
  forall (a :: Type) (se :: Type) (m :: Type -> Type).
  ( Monad m
  , NFData se
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Sampling function: From input to sample element (a "measurement").
  (a -> se) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m [SSample [se]]
benchSizesRandom
  randomGen
  sampleFun
  sampleSizePerInputSize
  sizes =
    -- using StateGenM to be able to freeze the seed. MonadRandom can't do this..
    runStateGenT_ (mkStdGen 42) . const $
      mapM (benchInputSize sampleSizePerInputSize) sizes
    where
      benchInputSize sampleSize inputSize = do
        inputs <- replicateM sampleSize (applyRandomGenM (randomGen inputSize) StateGenM)
        let sample = parMap rdeepseq sampleFun inputs
        pure $ SSample {inputSize, coverage = Nothing, sampleSize, sample}

genRandomDedup ::
  forall (g :: Type) (a :: Type) (m :: Type -> Type) (s :: Type).
  ( MonadPrim s m
  , RandomGen g
  , Hashable a
  ) =>
  Int ->
  (g -> (a, g)) ->
  StateT g m [a]
genRandomDedup sampleSize gen = do
  ht <- stToPrim $ HashTable.newSized sampleSize
  let loop 0 = pure []
      loop n = do
        input <- applyRandomGenM gen StateGenM
        isNew <-
          -- just abusing this mutable hashtable as a set
          -- lookup and insert at the same time
          stToPrim $
            HashTable.mutate ht input $
              maybe (Just (), True) (\() -> (Just (), False))
        if isNew
          then (input :) <$> loop (n - 1)
          else loop n
  loop sampleSize

genRandomSubset ::
  forall (g :: Type) (a :: Type) (m :: Type -> Type) (s :: Type).
  ( MonadPrim s m
  , RandomGen g
  ) =>
  Natural ->
  Int ->
  [a] ->
  StateT g m [a]
genRandomSubset card sampleSize set = do
  let dropNum = fromIntegral card - sampleSize
  -- fill a hashtable with dropNum indices into exhaustiveGen output
  ht <- stToPrim $ HashTable.newSized sampleSize
  let loop (-1) = pure ()
      loop n = do
        input :: Int <- uniformRM (0, fromIntegral card - 1) StateGenM
        isNew <-
          -- just abusing this mutable hashtable as a set
          -- lookup and insert at the same time
          stToPrim $
            HashTable.mutate ht input $
              maybe (Just (), True) (\() -> (Just (), False))
        if isNew
          then loop (n - 1)
          else loop n
  loop dropNum
  let es = zip [0 ..] set
  es' <- flip filterM es $ \(ix, _) ->
    stToPrim $ isNothing <$> HashTable.lookup ht ix
  pure $ snd <$> es'
