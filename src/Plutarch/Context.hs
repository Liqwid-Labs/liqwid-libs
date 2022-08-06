{- | Module: Plutarch.Context
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Base builder and other specific builders.
-}
module Plutarch.Context (
    B.Builder (..),
    B.BaseBuilder (..),
    B.UTXO (..),
    B.output,
    B.input,
    B.reference,
    B.credential,
    B.pubKey,
    B.script,
    B.withTxId,
    B.withDatum,
    B.withInlineDatum,
    B.withValue,
    B.withRefIndex,
    B.withOutRef,
    B.signedWith,
    B.mint,
    B.extraData,
    B.txId,
    B.fee,
    B.unpack,
    B.timeRange,
    B.mkOutRefIndices,
    S.SpendingBuilder (..),
    S.withSpendingUTXO,
    S.withSpendingOutRef,
    S.withSpendingOutRefId,
    S.withSpendingOutRefIdx,
    S.buildSpending,
    S.checkBuildSpending,
    S.checkSpending,
    M.MintingBuilder (..),
    M.withMinting,
    M.buildMinting,
    M.checkBuildMinting,
    T.TxInfoBuilder (..),
    T.spends,
    T.mints,
    T.buildTxInfo,
    Sub.buildTxOut,
    Sub.buildTxInInfo,
    Sub.buildTxOuts,
    Sub.buildTxInInfos,
    Sub.buildDatumHashPairs,
    P1.Checker (..),
    P1.CheckerErrorType (..),
    P1.updatePos,
    P1.basicError,
    P1.checkConst,
    P1.checkAt,
    P1.checkFoldable,
    P1.checkIf,
    P1.checkBool,
    P1.checkWith,
    P1.checkIfWith,
    P1.checkByteString,
    P1.checkValue,
    P1.checkTxId,
    P1.checkSignatures,
    P1.checkZeroSum,
    P1.checkInputs,
    P1.checkReferenceInputs,
    P1.checkFail,
    P1.checkMints,
    P1.checkFee,
    P1.checkOutputs,
    P1.checkDatumPairs,
    P1.checkPhase1,
    P1.renderErrors,
    P1.flattenValue,
) where

import qualified Plutarch.Context.Base as B
import qualified Plutarch.Context.Check as P1
import qualified Plutarch.Context.Minting as M
import qualified Plutarch.Context.Spending as S
import qualified Plutarch.Context.SubBuilder as Sub
import qualified Plutarch.Context.TxInfo as T
