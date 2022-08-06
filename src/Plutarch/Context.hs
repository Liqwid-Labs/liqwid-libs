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
    B.timeRange,
    S.SpendingBuilder (..),
    S.withSpendingUTXO,
    S.withSpendingOutRef,
    S.withSpendingOutRefId,
    S.withSpendingOutRefIdx,
    S.buildSpending,
    S.checkBuildSpending,
    S.buildSpendingUnsafe,
    M.MintingBuilder (..),
    M.withMinting,
    M.buildMinting,
    M.checkBuildMinting,
    M.buildMintingUnsafe,
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
    P1.checkPhase1,
    -- P1.checkFoldable,
    -- P1.checkIf,
    -- P1.checkByteString,
    -- P1.checkValue,
    -- P1.checkTxId,
    -- P1.checkSignatures,
    -- P1.checkZeroSum,
    -- P1.checkInputs,
    -- P1.checkOutputs,
    -- P1.checkDatumPairs,
    -- P1.checkPhase1,
) where

import qualified Plutarch.Context.Base as B
import qualified Plutarch.Context.Minting as M
import qualified Plutarch.Context.Check as P1
import qualified Plutarch.Context.Spending as S
import qualified Plutarch.Context.SubBuilder as Sub
import qualified Plutarch.Context.TxInfo as T
