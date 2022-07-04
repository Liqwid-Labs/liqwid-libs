{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.Benchmark.Cost (
  CostVectors (..),
  SimpleStats (..),
  vecSimpleStats,
  cvSimpleStats,
) where

import Control.Foldl qualified as Foldl
import Data.Csv (ToNamedRecord, namedRecord, toNamedRecord, (.=))
import Data.Maybe (fromJust)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)

-- | Holds unboxed Vectors of Double, for use with statistics libraries.
newtype CostVectors axis = CostVectors [(axis, Vector Double)]
  deriving stock (Show, Eq, Generic)

data SimpleStats = SimpleStats
  { minVal :: Double
  , meanVal :: Double
  , maxVal :: Double
  , stddev :: Double
  }
  deriving stock (Show, Eq, Generic)

instance ToNamedRecord SimpleStats where
  toNamedRecord (SimpleStats {..}) =
    namedRecord
      [ "min" .= minVal
      , "mean" .= meanVal
      , "max" .= maxVal
      , "stddev" .= stddev
      ]

vecSimpleStats :: Vector Double -> SimpleStats
vecSimpleStats vec = flip Foldl.fold (Vector.toList vec) $ do
  -- TODO maybe use foldl-statistics
  minVal <- fromJust <$> Foldl.minimum
  maxVal <- fromJust <$> Foldl.maximum
  meanVal <- Foldl.mean
  stddev <- Foldl.std
  pure $ SimpleStats {..}

cvSimpleStats :: CostVectors axis -> [(axis, SimpleStats)]
cvSimpleStats (CostVectors cvs) = map (fmap vecSimpleStats) cvs
