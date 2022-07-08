{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.Benchmark.Cost (
  AxisMap (..),
  axes,
  axisLookup,
  CostAxis,
  axisName,
  BudgetExceeded (..),
  CostVector (..),
  SimpleStats (..),
  vecSimpleStats,
  cvSimpleStats,
  samplesToPerAxisStats,
) where

import Control.Foldl qualified as Foldl
import Control.Monad (void)
import Data.Csv (
  DefaultOrdered (headerOrder),
  ToNamedRecord,
  header,
  namedRecord,
  toNamedRecord,
  (.=),
 )
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import Optics (sequenceOf, traversed, (%%))
import Test.Benchmark.Sized (SSample (SSample), sample, sampleSize)

class CostAxis a where
  axisName :: a -> Text
  axes :: [a]
  default axisName :: Show a => a -> Text
  axisName a = pack $ show a
  default axes :: (Bounded a, Enum a) => [a]
  axes = enumFromTo minBound maxBound

{- | Holds values for all your cost axes.

 All axes always have a value in this Map. If you want a subset of your axes,
 use a different type.
-}
newtype AxisMap a b = AxisMap [(a, b)]
  deriving stock (Functor)

instance (CostAxis a, Eq a) => Applicative (AxisMap a) where
  pure x = AxisMap $ map (,x) axes
  f <*> b =
    AxisMap $
      map
        (\axis -> (axis, axisLookup axis f $ axisLookup axis b))
        axes

axisLookup :: (CostAxis a, Eq a) => a -> AxisMap a b -> b
axisLookup axis (AxisMap pairs) = fromJust $ lookup axis pairs

newtype BudgetExceeded axis = BudgetExceeded {exceededAxis :: axis}
  deriving stock (Show, Eq, Generic)

-- | Unboxed Vector of Double, for use with statistics libraries.
newtype CostVector = CostVector (Vector Double)

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

instance DefaultOrdered SimpleStats where
  headerOrder _ =
    header
      [ "min"
      , "mean"
      , "max"
      , "stddev"
      ]

instance
  CostAxis axis =>
  ToNamedRecord (SSample (Either (BudgetExceeded axis) SimpleStats))
  where
  toNamedRecord ssample@(SSample {sample = Left (BudgetExceeded axis)}) =
    namedRecord $
      HashMap.toList (toNamedRecord (void ssample))
        <> [ "budget" .= (axisName axis <> "!")
           , "min" .= ("" :: String)
           , "mean" .= ("" :: String)
           , "max" .= ("" :: String)
           , "stddev" .= ("" :: String)
           ]
  toNamedRecord ssample@(SSample {sample = Right simpleStats}) =
    namedRecord $
      HashMap.toList (toNamedRecord (void ssample))
        <> ["budget" .= ("" :: String)]
        <> HashMap.toList (toNamedRecord simpleStats)

instance
  CostAxis axis =>
  DefaultOrdered (SSample (Either (BudgetExceeded axis) SimpleStats))
  where
  headerOrder ssample =
    headerOrder (void ssample)
      <> header ["budget"]
      <> headerOrder (undefined :: SimpleStats)

vecSimpleStats :: CostVector -> SimpleStats
vecSimpleStats (CostVector vec) = flip Foldl.fold (Vector.toList vec) $ do
  -- TODO maybe use foldl-statistics
  minVal <- fromJust <$> Foldl.minimum
  maxVal <- fromJust <$> Foldl.maximum
  meanVal <- Foldl.mean
  stddev <- Foldl.std
  pure $ SimpleStats {..}

cvSimpleStats :: AxisMap axis CostVector -> AxisMap axis SimpleStats
cvSimpleStats = fmap vecSimpleStats

{- | Convert 'benchSizes..' result for statistics libraries.

 See the 'benchSizes..' functions in 'Test.Benchmark.Sized'.

 Statistics libraries love Vectors of Doubles for some reason. Makes per-axis
 cost vectors. Treats the each per-input-size sample as invalid if one of the
 sample elements had an exceeded budget.

 Composing this with 'benchSizes..' and friends also ensures that no references
 to the list are kept, allowing immediate GC on it.
-}
costSampleToVectors ::
  (Int -> [sampleElem] -> AxisMap axis CostVector) ->
  SSample [Either (BudgetExceeded axis) sampleElem] ->
  SSample (Either (BudgetExceeded axis) (AxisMap axis CostVector))
costSampleToVectors toVecs ssample@SSample {sampleSize, sample = eCosts} =
  let eCostVecs = toVecs sampleSize <$> sequence eCosts
   in ssample {sample = eCostVecs}

-- | Postprocesses 'benchSizes..' output into per-axis statistics.
samplesToPerAxisStats ::
  (CostAxis axis, Eq axis) =>
  -- | The 'vecsFun': Converts sample elements (also given the sample size) to
  -- cost vectors.
  (Int -> [sampleElem] -> AxisMap axis CostVector) ->
  -- | The 'statsFun': Makes per-axis statistics.
  (CostVector -> stats) ->
  -- | The raw per-input-size samples out of 'benchSizesUniversal' and friends.
  [SSample [Either (BudgetExceeded axis) sampleElem]] ->
  AxisMap axis [SSample (Either (BudgetExceeded axis) stats)]
samplesToPerAxisStats vecsFun statsFun ssamples =
  sequenceOf (traversed %% traversed %% traversed) sAllStats
  where
    scvs = fmap (costSampleToVectors vecsFun) ssamples
    sAllStats = (fmap . fmap . fmap . fmap) statsFun scvs