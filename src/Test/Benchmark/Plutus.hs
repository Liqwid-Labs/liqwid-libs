{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Test.Benchmark.Plutus (
  ImplMetaData (..),
  mkScriptImplMetaData,
  PlutusCostAxis (..),
  Cost (..),
  BudgetExceeded (..),
  Costs (..),
  ScriptFailure (..),
  sampleScript',
  sampleScript,
  sampleDScript,
  statsByAxis',
  statsByAxis,
) where

import Codec.Serialise (serialise)
import Control.Parallel.Strategies (NFData)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Base (Vector (V_2))
import GHC.Generics (Generic)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Evaluate (evalScript)
import PlutusCore.Evaluation.Machine.ExBudget (
  ExRestrictingBudget (ExRestrictingBudget),
 )
import PlutusCore.Evaluation.Machine.Exception (
  ErrorWithCause (..),
  EvaluationError (InternalEvaluationError, UserEvaluationError),
 )
import PlutusLedgerApi.V1 (
  ExBudget (ExBudget),
  ExCPU (..),
  ExMemory (..),
  Script,
 )
import PlutusLedgerApi.V1.Scripts (Script (Script))
import Test.Benchmark.Common (ImplData)
import Test.Benchmark.Cost (
  AxisMap (AxisMap),
  BudgetExceeded (BudgetExceeded),
  CostAxis,
  CostVector (..),
  SimpleStats,
  samplesToPerAxisStats,
  vecSimpleStats,
 )
import Test.Benchmark.DScript (DScript (DScript), debugScript, script)
import Test.Benchmark.Sized (SSample)
import qualified UntypedPlutusCore as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek (
  CekUserError (CekEvaluationFailure, CekOutOfExError),
 )

-- TODO add script hash, maybe also git commit hash, mtime
-- TODO actually make use of this and write to some file
data ImplMetaData = ImplMetaData
  { name :: Text
  -- ^ Name of the implementation. Make sure it's unique.
  , scriptSize :: Integer
  -- ^ Size of the script without inputs (number of AST nodes)
  , scriptSizeBytes :: Int
  -- ^ Size of the script without inputs (serialized in bytes)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''ImplMetaData

mkScriptImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  Script ->
  ImplMetaData
mkScriptImplMetaData name script = ImplMetaData {name, scriptSize, scriptSizeBytes}
  where
    Script uplcProg = script
    scriptSize = UPLC.programSize uplcProg
    scriptSizeBytes = fromIntegral . LBS.length . serialise $ script

data PlutusCostAxis = CPU | Mem
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (CostAxis, NFData)

-- | Based on Int, since the Plutus budget types are Int internally as well
newtype Cost (a :: PlutusCostAxis) = Cost {value :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Cost

data Costs = Costs
  { cpuCost :: Cost 'CPU
  , memCost :: Cost 'Mem
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Costs

newtype ScriptFailure = ScriptFailure {traces :: [Text]}

makeFieldLabelsNoPrefix ''ScriptFailure

-- | Sample Script execution, crash on evaluation failure.
sampleScript :: Script -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleScript script =
  case sampleScript' script of
    Right costs -> Right costs
    Left failure ->
      case failure of
        Left (ScriptFailure traces) ->
          error $
            "Script failed for non-budget reason!\n"
              <> Text.unpack (Text.unlines traces)
        Right budgetExceeded -> Left budgetExceeded

-- | Sample Script execution, try debug variant of Script on evaluation failure.
sampleDScript :: DScript -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleDScript DScript {script, debugScript} =
  case sampleScript' script of
    Right costs -> Right costs
    Left failure ->
      case failure of
        Left _ -> sampleScript debugScript
        Right budgetExceeded -> Left budgetExceeded

sampleScript' :: Script -> Either (Either ScriptFailure (BudgetExceeded PlutusCostAxis)) Costs
sampleScript' script =
  case res of
    Right _ -> pure $ Costs {cpuCost, memCost}
    Left (ErrorWithCause evalErr _) ->
      case evalErr of
        InternalEvaluationError _ -> error "Internal evaluation-error!"
        UserEvaluationError e ->
          case e of
            CekEvaluationFailure ->
              Left . Left $ ScriptFailure traces
            CekOutOfExError (ExRestrictingBudget (ExBudget rcpu rmem)) ->
              if rcpu < 0
                then Left . Right $ BudgetExceeded CPU
                else
                  if rmem < 0
                    then Left . Right $ BudgetExceeded Mem
                    else
                      error $
                        "Got CekOutOfExError, but ExRestrictingBudget contains "
                          <> "neither negative CPU nor negative Memory!"
  where
    (res, ExBudget (ExCPU rawCpu) (ExMemory rawMem), traces) = evalScript script
    cpuCost = Cost $ fromIntegral rawCpu
    memCost = Cost $ fromIntegral rawMem

plutusCostsToVecs :: Int -> [Costs] -> AxisMap PlutusCostAxis CostVector
plutusCostsToVecs sampleSize costs =
  AxisMap [(CPU, CostVector cpuVec), (Mem, CostVector memVec)]
  where
    -- Vector.Unboxed stores in columns
    V_2 _ cpuVec memVec = toVec costs
    toVec :: [Costs] -> Vector (Double, Double)
    toVec = Vector.fromListN sampleSize . map costsToPair
    costsToPair (Costs (Cost cpu) (Cost mem)) =
      (fromIntegral cpu, fromIntegral mem)

-- | Postprocesses 'benchSizes..' output into per-axis statistics.
statsByAxis' ::
  [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]] ->
  AxisMap
    PlutusCostAxis
    [SSample (Either (BudgetExceeded PlutusCostAxis) SimpleStats)]
statsByAxis' =
  samplesToPerAxisStats plutusCostsToVecs vecSimpleStats

-- | Postprocesses 'benchSizes..' output into per-axis statistics.
statsByAxis ::
  ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]] ->
  ImplData
    ( AxisMap
        PlutusCostAxis
        [SSample (Either (BudgetExceeded PlutusCostAxis) SimpleStats)]
    )
statsByAxis = fmap statsByAxis'
