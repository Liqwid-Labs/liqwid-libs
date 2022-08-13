{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | @since 1.0.0
module Plutarch.Benchmark.Cost (
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
  writePerAxisCSVs,
  rankOnPerAxisStat,
  writeComparisonPerAxisCSVs,
) where

import qualified Control.Foldl as Foldl
import Control.Monad (forM_, void)
import Control.Parallel.Strategies (NFData)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv (
  DefaultOrdered (headerOrder),
  EncodeOptions (encUseCrLf),
  ToField (toField),
  ToNamedRecord,
  defaultEncodeOptions,
  encodeByNameWith,
  encodeDefaultOrderedByNameWith,
  header,
  namedRecord,
  toNamedRecord,
  (.=),
 )
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith)
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import GHC.Generics (Generic)
import Optics (sequenceOf, traversed, (%%))
import Optics.TH (makeFieldLabelsNoPrefix)
import Path (Dir, Path, Rel, parseRelFile, toFilePath, (</>))
import Plutarch.Benchmark.Common (
  ImplData (ImplData, name, val),
  MultiImplComparisonData (
    MultiImplComparisonData,
    comparisonName,
    implNames,
    name,
    val
  ),
  MultiImplData (MultiImplData, implNames, name, val),
  ToTitle (toTitle),
 )
import Plutarch.Benchmark.Sized (
  SSample (SSample),
  coverage,
  inputSize,
  sample,
  sampleSize,
 )

class Eq (a :: Type) => CostAxis a where
  axisName :: a -> Text
  axes :: [a]
  default axisName :: Show a => a -> Text
  axisName a = pack $ show a
  default axes :: (Bounded a, Enum a) => [a]
  axes = [minBound .. maxBound]

{- | Holds values for all your cost axes.

 All axes always have a value in this Map. If you want a subset of your axes,
 use a different type.

 @since 1.0.0
-}
newtype AxisMap (a :: Type) (b :: Type) = AxisMap {pairs :: [(a, b)]}
  deriving stock (Show, Functor, Generic)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''AxisMap

-- | @since 1.0.0
instance forall (a :: Type). (CostAxis a) => Applicative (AxisMap a) where
  pure x = AxisMap $ map (,x) axes
  f <*> b =
    AxisMap $
      map
        (\axis -> (axis, axisLookup axis f $ axisLookup axis b))
        axes

-- | @since 1.0.0
axisLookup :: forall (a :: Type) (b :: Type). (CostAxis a) => a -> AxisMap a b -> b
axisLookup axis (AxisMap pairs) = fromJust $ lookup axis pairs

-- | @since 1.0.0
newtype BudgetExceeded (axis :: Type) = BudgetExceeded {exceededAxis :: axis}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Unboxed Vector of Double, for use with statistics libraries.
-- | @since 1.0.0
newtype CostVector = CostVector (Vector Double)

-- | @since 1.0.0
data SimpleStats = SimpleStats
  { minVal :: Double
  , meanVal :: Double
  , maxVal :: Double
  , stddev :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''SimpleStats

-- | @since 1.0.0
instance ToNamedRecord SimpleStats where
  toNamedRecord (SimpleStats {..}) =
    namedRecord
      [ "min" .= minVal
      , "mean" .= meanVal
      , "max" .= maxVal
      , "stddev" .= stddev
      ]

-- | @since 1.0.0
instance DefaultOrdered SimpleStats where
  headerOrder _ =
    header
      [ "min"
      , "mean"
      , "max"
      , "stddev"
      ]

-- | @since 1.0.0
instance
  forall (axis :: Type).
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

-- | @since 1.0.0
instance
  forall (axis :: Type) (s :: Type).
  (CostAxis axis, ToField s) =>
  ToNamedRecord (SSample [ImplData (Either (BudgetExceeded axis) s)])
  where
  toNamedRecord ssample@(SSample {sample = ranks}) =
    namedRecord $
      HashMap.toList (toNamedRecord (void ssample))
        <> zipWith
          (\place implData -> fromString (show place) .= implData)
          [1 :: Int ..]
          ranks

-- | @since 1.0.0
instance
  forall (axis :: Type) (s :: Type).
  ( CostAxis axis
  , ToField s
  ) =>
  ToField (Either (BudgetExceeded axis) s)
  where
  toField (Left (BudgetExceeded axis)) = encodeUtf8 (axisName axis) <> "!"
  toField (Right s) = toField s

-- | @since 1.0.0
instance
  forall (axis :: Type).
  CostAxis axis =>
  DefaultOrdered (SSample (Either (BudgetExceeded axis) SimpleStats))
  where
  headerOrder ssample =
    headerOrder (void ssample)
      <> header ["budget"]
      <> headerOrder (undefined :: SimpleStats)

-- | @since 1.0.0
vecSimpleStats :: CostVector -> SimpleStats
vecSimpleStats (CostVector vec) = flip Foldl.fold (Vector.toList vec) $ do
  -- TODO maybe use foldl-statistics
  minVal <- fromJust <$> Foldl.minimum
  maxVal <- fromJust <$> Foldl.maximum
  meanVal <- Foldl.mean
  stddev <- Foldl.std
  pure $ SimpleStats {..}

-- | @since 1.0.0
cvSimpleStats :: forall (axis :: Type). AxisMap axis CostVector -> AxisMap axis SimpleStats
cvSimpleStats = fmap vecSimpleStats

{- | Convert 'benchSizes..' result for statistics libraries.

 See the 'benchSizes..' functions in 'Test.Benchmark.Sized'.

 Statistics libraries love Vectors of Doubles for some reason. Makes per-axis
 cost vectors. Treats the each per-input-size sample as invalid if one of the
 sample elements had an exceeded budget.

 Composing this with 'benchSizes..' and friends also ensures that no references
 to the list are kept, allowing immediate GC on it.

 @since 1.0.0
-}
costSampleToVectors ::
  forall (sampleElem :: Type) (axis :: Type).
  (Int -> [sampleElem] -> AxisMap axis CostVector) ->
  SSample [Either (BudgetExceeded axis) sampleElem] ->
  SSample (Either (BudgetExceeded axis) (AxisMap axis CostVector))
costSampleToVectors toVecs ssample@SSample {sampleSize, sample = eCosts} =
  let eCostVecs = toVecs sampleSize <$> sequence eCosts
   in ssample {sample = eCostVecs}

-- | Postprocesses 'benchSizes..' output into per-axis statistics.
-- | @since 1.0.0
samplesToPerAxisStats ::
  forall (axis :: Type) (sampleElem :: Type) (stats :: Type).
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

-- | Rank multiple implementations by some statistic.
-- | @since 1.0.0
rankOnPerAxisStat ::
  forall (axis :: Type) (stats :: Type) (s :: Type).
  (CostAxis axis, Eq axis, Ord s) =>
  -- | Name of the ranking value, i.e. "mean" or "worst case"
  Text ->
  -- | Selects the statistic value to rank by from the stats.
  (stats -> s) ->
  -- | Per implementation: per axis: size-specific samples containing the stats
  MultiImplData
    [ImplData (AxisMap axis [SSample (Either (BudgetExceeded axis) stats)])] ->
  MultiImplComparisonData
    (AxisMap axis [SSample [ImplData (Either (BudgetExceeded axis) s)]])
rankOnPerAxisStat
  comparisonName
  sel
  MultiImplData {name, implNames, val = statsPerImpl} =
    MultiImplComparisonData {name, implNames, comparisonName, val = sorted}
    where
      -- raise the axis mapping to the top level
      raisedMap ::
        AxisMap axis [ImplData [SSample (Either (BudgetExceeded axis) stats)]]
      raisedMap = sequenceOf (traversed %% traversed) statsPerImpl

      -- lower the impl name to each SSample
      loweredName ::
        AxisMap axis [ImplData (SSample (Either (BudgetExceeded axis) stats))]
      loweredName = concat <$> (fmap . fmap) sequence raisedMap

      -- swap the impl name into the SSample.
      -- Now we have multiple SSamples for each input size in the per-axis list.
      swapped ::
        AxisMap axis [SSample (ImplData (Either (BudgetExceeded axis) stats))]
      swapped = (fmap . fmap) sequenceSamp loweredName
      -- a workaround for Applicative on SSample not being possible
      sequenceSamp ::
        forall (a :: Type). ImplData (SSample a) -> SSample (ImplData a)
      sequenceSamp implData =
        implData.val
          { sample =
              ImplData
                { name = implData.name
                , val = implData.val.sample
                }
          }

      -- We merge the SSamples for each input size, keeping the minimum coverage
      -- and sample size values.
      merged ::
        AxisMap axis [SSample [ImplData (Either (BudgetExceeded axis) stats)]]
      merged = fmap (fmap mergeSSamples . groupAllWith (.inputSize)) swapped

      mergeSSamples :: forall (a :: Type). NonEmpty (SSample a) -> SSample [a]
      mergeSSamples (s0 :| ss) = foldl' mergeTwo (fmap (: []) s0) ss
      mergeTwo :: forall (a :: Type). SSample [a] -> SSample a -> SSample [a]
      mergeTwo sAccum s =
        if sAccum.inputSize /= s.inputSize
          then error "tried to merge SSamples with differing input size"
          else
            SSample
              { inputSize = sAccum.inputSize
              , sampleSize = min sAccum.sampleSize s.sampleSize
              , coverage = min sAccum.coverage s.coverage
              , sample = s.sample : sAccum.sample
              }

      -- Use sel to focus on a single stat
      selected = (fmap . fmap . fmap . fmap . fmap . fmap) sel merged
      -- Sort the list in each SSample by this stat
      sorted = (fmap . fmap . fmap) (sortBy (cmp `on` (.val))) selected
      -- BudgetExceeded is always considered bigger, compared to an actual value
      -- (contrary to how Either compares by default)
      cmp (Left _) (Right _) = GT
      cmp (Right _) (Left _) = LT
      cmp (Left _) (Left _) = EQ
      cmp (Right a) (Right b) = compare a b

-- | Write per-axis titled data to CSV files.
-- | @since 1.0.0
writePerAxisCSVs ::
  forall (a :: Type) (d :: Type).
  ( CostAxis a
  , DefaultOrdered d
  , ToNamedRecord d
  ) =>
  -- | Output directory path.
  Path Rel Dir ->
  -- | The per-axis data.
  ImplData (AxisMap a [d]) ->
  IO ()
writePerAxisCSVs dir titled = do
  let AxisMap pairs =
        encodeDefaultOrderedByNameWith defaultEncodeOptions {encUseCrLf = False}
          <$> titled.val
  forM_ pairs $ \(axis, csvData) -> do
    file <-
      parseRelFile . unpack $ toTitle titled <> " " <> axisName axis <> ".csv"
    ByteString.writeFile (toFilePath $ dir </> file) csvData

{- | Write per-axis implementation comparison data to CSV files.

 This exists because 'DefaultOrdered' for the column headers can't be
 implemented in this case, since the number of compared implementations isn't
 fixed, and we need one column per rank.

 @since 1.0.0
-}
writeComparisonPerAxisCSVs ::
  forall (a :: Type) (d :: Type).
  ( CostAxis a
  , ToNamedRecord d
  ) =>
  -- | Output directory path.
  Path Rel Dir ->
  -- | The comparison data.
  MultiImplComparisonData (AxisMap a [d]) ->
  IO ()
writeComparisonPerAxisCSVs dir compData = do
  let AxisMap pairs =
        encodeByNameWith defaultEncodeOptions {encUseCrLf = False} header'
          <$> compData.val
      header' =
        headerOrder (undefined :: SSample ())
          <> header (map (fromString . show) [1 .. (length compData.implNames)])
  forM_ pairs $ \(axis, csvData) -> do
    file <-
      parseRelFile . unpack $ toTitle compData <> " " <> axisName axis <> ".csv"
    ByteString.writeFile (toFilePath $ dir </> file) csvData
