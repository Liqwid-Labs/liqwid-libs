{-# LANGUAGE NoFieldSelectors #-}

module Test.Benchmark.Plutus (
  ImplMetaData (..),
  mkScriptImplMetaData,
  CostAxis (..),
  Cost (..),
  BudgetExceeded (..),
  Costs (..),
  sampleScript,
  costSampleToVectors,
) where

import Codec.Serialise (serialise)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Csv (ToNamedRecord, namedRecord, toNamedRecord, (.=))
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Base (Vector (V_2))
import GHC.Generics (Generic)
import Plutarch.Evaluate (evalScript)
import PlutusCore.Evaluation.Machine.ExBudget (ExRestrictingBudget (ExRestrictingBudget))
import PlutusCore.Evaluation.Machine.Exception (ErrorWithCause (..), EvaluationError (InternalEvaluationError, UserEvaluationError))
import PlutusLedgerApi.V1 (
  ExBudget (ExBudget),
  ExCPU (..),
  ExMemory (..),
  Script,
 )
import Test.Benchmark.Cost (CostVectors (..), SimpleStats)
import Test.Benchmark.Sized (SSample (SSample), sample, sampleSize)
import UntypedPlutusCore.Evaluation.Machine.Cek (CekUserError (CekEvaluationFailure, CekOutOfExError))

data ImplMetaData = ImplMetaData
  { name :: Text
  -- ^ Name of the implementation. Make sure it's unique.
  , scriptSize :: Int
  -- ^ Size of the script without inputs.
  }
  deriving stock (Show, Eq, Ord, Generic)

mkScriptImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  Script ->
  ImplMetaData
mkScriptImplMetaData name script = ImplMetaData {name, scriptSize}
  where
    scriptSize = fromIntegral . LBS.length . serialise $ script

data CostAxis = CPU | Mem deriving stock (Show, Eq, Ord, Generic)

-- | Based on Int, since the Plutus budget types are Int internally as well
newtype Cost (a :: CostAxis) = Cost {value :: Int}
  deriving stock (Show, Eq, Ord, Generic)

data Costs = Costs
  { cpuCost :: Cost 'CPU
  , memCost :: Cost 'Mem
  }
  deriving stock (Show, Eq, Generic)

newtype BudgetExceeded = BudgetExceeded {exceededAxis :: CostAxis} deriving stock (Show, Eq, Generic)

sampleScript :: Script -> Either BudgetExceeded Costs
sampleScript script =
  case res of
    Right _ -> pure $ Costs {cpuCost, memCost}
    Left (ErrorWithCause evalErr _) ->
      case evalErr of
        InternalEvaluationError _ -> error "Internal evaluation-error!"
        UserEvaluationError e ->
          case e of
            CekEvaluationFailure -> error "Script failed for non-budget reason!"
            CekOutOfExError (ExRestrictingBudget (ExBudget rcpu rmem)) ->
              if rcpu < 0
                then Left $ BudgetExceeded CPU
                else
                  if rmem < 0
                    then Left $ BudgetExceeded Mem
                    else
                      error $
                        "Got CekOutOfExError, but ExRestrictingBudget contains "
                          <> "neither negative CPU nor negative Memory!"
  where
    (res, ExBudget (ExCPU rawCpu) (ExMemory rawMem), _traces) = evalScript script
    cpuCost = Cost $ fromIntegral rawCpu
    memCost = Cost $ fromIntegral rawMem

{- | Convert 'benchSizes' result for statistics libraries.

 Statistics libraries love Vectors of Doubles for some reason.
 Composing this with 'benchSizes' also ensures that no references
 to the list are kept, allowing immediate GC on it.
-}
costSampleToVectors ::
  SSample [Either BudgetExceeded Costs] ->
  SSample (Either BudgetExceeded (CostVectors CostAxis))
costSampleToVectors ssample@SSample {sampleSize, sample = eCosts} =
  let eCostVecs = toVecs <$> sequence eCosts
      toVecs costs =
        -- Vector.Unboxed stores in columns
        let V_2 _ cpuVec memVec = toVec costs
         in CostVectors [(CPU, cpuVec), (Mem, memVec)]
      toVec :: [Costs] -> Vector (Double, Double)
      toVec = Vector.fromListN sampleSize . map costsToPair
      costsToPair (Costs (Cost cpu) (Cost mem)) =
        (fromIntegral cpu, fromIntegral mem)
   in ssample {sample = eCostVecs}

instance ToNamedRecord (SSample (Either BudgetExceeded SimpleStats)) where
  toNamedRecord ssample@(SSample {sample = Left (BudgetExceeded axis)}) =
    namedRecord $
      HashMap.toList (toNamedRecord (void ssample))
        <> ["budget" .= (show axis <> "!")]
        <> [ "min" .= ("" :: String)
           , "mean" .= ("" :: String)
           , "max" .= ("" :: String)
           , "stddev" .= ("" :: String)
           ]
  toNamedRecord ssample@(SSample {sample = Right simpleStats}) =
    namedRecord $
      HashMap.toList (toNamedRecord (void ssample))
        <> HashMap.toList (toNamedRecord simpleStats)
        <> ["budget" .= ("" :: String)]