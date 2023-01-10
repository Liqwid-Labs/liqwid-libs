{-# LANGUAGE RankNTypes #-}

{- | Bridge between QuickCheck generators and System.Random

 Inspired by hedgehog-quickcheck.

 @since 1.0.0
-}
module Plutarch.Benchmark.QuickCheck (
  genToRand,
  randToGen,
) where

import Data.Kind (Type)
import System.Random (RandomGen, mkStdGen)
import System.Random.Stateful (Uniform (uniformM), runStateGen)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen (MkGen), sized)
import Test.QuickCheck.Random (mkQCGen)

{- | Converts a QuickCheck Gen to a pure function in terms of
 System.Random.RandomGen

 @since 1.0.0
-}
genToRand ::
  forall (a :: Type) (g :: Type).
  RandomGen g =>
  Gen a ->
  Int ->
  g ->
  (a, g)
genToRand (MkGen runQcGen) size g = runStateGen g $ \stateGen -> do
  qcGen <- mkQCGen <$> uniformM stateGen
  pure $ runQcGen qcGen size

{- | Converts a pure function in terms of System.Random.RandomGen to a
 QuickCheck Gen

@since 1.0.0
-}
randToGen ::
  forall (a :: Type).
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  Gen a
randToGen pureRand = do
  g <- mkStdGen <$> arbitrary
  sized $ \size -> pure $ fst $ pureRand size g
