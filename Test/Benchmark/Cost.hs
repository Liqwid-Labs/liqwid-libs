{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
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
  writePerAxisCSVs,
  rankOnPerAxisStat,
  writeComparisonPerAxisCSVs,
) where

import Control.Foldl qualified as Foldl
import Control.Monad (forM_, void)
import Data.ByteString.Lazy qualified as ByteString
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
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)), groupWith)
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Optics (sequenceOf, traversed, (%%))
import Optics.TH (makeFieldLabelsNoPrefix)
import Path (Dir, Path, Rel, parseRelFile, toFilePath, (</>))
import Test.Benchmark.Common (
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
import Test.Benchmark.Sized (
  SSample (SSample),
  coverage,
  inputSize,
  sample,
  sampleSize,
 )

class Eq a => CostAxis a where
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
newtype AxisMap a b = AxisMap {pairs :: [(a, b)]}
  deriving stock (Show, Functor, Generic)

makeFieldLabelsNoPrefix ''AxisMap

instance (CostAxis a) => Applicative (AxisMap a) where
  pure x = AxisMap $ map (,x) axes
  f <*> b =
    AxisMap $
      map
        (\axis -> (axis, axisLookup axis f $ axisLookup axis b))
        axes

axisLookup :: (CostAxis a) => a -> AxisMap a b -> b
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

makeFieldLabelsNoPrefix ''SimpleStats

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

instance
  ( CostAxis axis
  , ToField s
  ) =>
  ToField (Either (BudgetExceeded axis) s)
  where
  toField (Left (BudgetExceeded axis)) = encodeUtf8 (axisName axis) <> "!"
  toField (Right s) = toField s

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

-- | Rank multiple implementations by some statistic.
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
rankOnPerAxisStat comparisonName sel MultiImplData {name, implNames, val = statsPerImpl} =
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
    merged :: AxisMap axis [SSample [ImplData (Either (BudgetExceeded axis) stats)]]
    merged = fmap (fmap mergeSSamples . groupWith (.inputSize)) swapped

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
writePerAxisCSVs ::
  ( CostAxis a
  , DefaultOrdered d
  , ToNamedRecord d
  , ToTitle n
  , HasField "val" n (AxisMap a [d])
  ) =>
  -- | Output directory path.
  Path Rel Dir ->
  -- | See the types in 'Test.Benchmark.Common'.
  n ->
  IO ()
writePerAxisCSVs dir titled = do
  let AxisMap pairs =
        encodeDefaultOrderedByNameWith defaultEncodeOptions {encUseCrLf = False}
          <$> titled.val
  forM_ pairs $ \(axis, csvData) -> do
    file <- parseRelFile . unpack $ toTitle titled <> " " <> axisName axis <> ".csv"
    ByteString.writeFile (toFilePath $ dir </> file) csvData

{- | Write per-axis implementation comparison data to CSV files.

 This exists because 'DefaultOrdered' for the column headers can't be
 implemented in this case, since the number of compared implementations isn't
 fixed, and we need one column per rank.
-}
writeComparisonPerAxisCSVs ::
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
    file <- parseRelFile . unpack $ toTitle compData <> " " <> axisName axis <> ".csv"
    ByteString.writeFile (toFilePath $ dir </> file) csvData