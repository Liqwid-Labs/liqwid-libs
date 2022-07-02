{-# LANGUAGE NoFieldSelectors #-}

module Test.Benchmark.Plutus (
  ImplMetaData (..),
  mkScriptImplMetaData,
  CostAxis (..),
  Cost (..),
  Costs (..),
  sampleScript,
) where

import Codec.Serialise (serialise)
import Control.Monad.ST (ST)
import Control.Monad.ST.Class (MonadST, liftST)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
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
newtype Cost (a :: CostAxis) = Cost Int deriving stock (Show, Eq, Ord, Generic)

data Costs
  = Costs
      { cpuCost :: Cost 'CPU
      , memCost :: Cost 'Mem
      }
  | BudgetExceeded {exceededAxis :: CostAxis}
  deriving stock (Show, Eq, Ord, Generic)

sampleScript :: Script -> Costs
sampleScript script =
  case res of
    Right _ -> Costs {cpuCost, memCost}
    Left (ErrorWithCause evalErr _) ->
      case evalErr of
        InternalEvaluationError _ -> error "Internal evaluation-error!"
        UserEvaluationError e ->
          case e of
            CekEvaluationFailure -> error "Script failed for non-budget reason!"
            CekOutOfExError (ExRestrictingBudget (ExBudget rcpu rmem)) ->
              if rcpu < 0
                then BudgetExceeded CPU
                else
                  if rmem < 0
                    then BudgetExceeded Mem
                    else
                      error $
                        "Got CekOutOfExError, but ExRestrictingBudget contains "
                          <> "neither negative CPU nor negative Memory!"
  where
    (res, ExBudget (ExCPU rawCpu) (ExMemory rawMem), _traces) = evalScript script
    cpuCost = Cost $ fromIntegral rawCpu
    memCost = Cost $ fromIntegral rawMem
