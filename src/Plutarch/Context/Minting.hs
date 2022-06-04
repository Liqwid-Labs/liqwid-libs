{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Minting
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for minting contexts. 'MintingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'MintingBuilder' using '<>'.
-}
module Plutarch.Context.Minting (
    MintingBuilder (..),
    buildMinting,
) where

import Control.Monad.Cont (ContT (runContT))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Foldable (Foldable (toList))
import Plutarch.Context.Base (
    BaseBuilder (bbDatums, bbInputs, bbMints, bbOutputs, bbSignatures),
    Builder (..),
    baseTxInfo,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldMint,
    yieldOutDatums,
 )

import Plutarch.Context.Config (ContextConfig)
import Plutarch.Lift (PUnsafeLiftDecl (..))
import PlutusLedgerApi.V1.Contexts (
    ScriptContext (ScriptContext),
    ScriptPurpose (Minting),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (ValidatorHash)
import PlutusLedgerApi.V1.Value (
    CurrencySymbol,
    Value,
 )
  
data MintingBuilder = MB
    { mbInner :: BaseBuilder
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

instance Semigroup MintingBuilder where
    MB inner <> MB inner' =
        MB (inner <> inner')

instance Monoid MintingBuilder where
    mempty = MB mempty

instance Builder MintingBuilder where
    pack = MB
    unpack = mbInner

uniformCurrencySymbol ::
    Value ->
    Maybe CurrencySymbol
uniformCurrencySymbol (Value.flattenValue -> xs)
    | and $ (== head syms) <$> syms = Just $ head syms
    | otherwise = Nothing
  where
    syms = (\(cs, _, _) -> cs) <$> xs

buildMinting ::
    ContextConfig ->
    MintingBuilder ->
    Maybe ScriptContext
buildMinting config builder = flip runContT Just $
    do
        let bb = unpack builder

        (ins, inDat) <- yieldInInfoDatums (bbInputs bb) config
        (outs, outDat) <- yieldOutDatums (bbOutputs bb)
        mintedValue <- yieldMint (bbMints bb)
        extraDat <- yieldExtraDatums (bbDatums bb)
        base <- baseTxInfo config

        let txinfo =
                base
                    { txInfoInputs = ins
                    , txInfoOutputs = outs
                    , txInfoData = inDat <> outDat <> extraDat
                    , txInfoMint = mintedValue
                    , txInfoSignatories = toList $ bbSignatures bb
                    }

        case uniformCurrencySymbol mintedValue of
            Just c -> return $ ScriptContext txinfo (Minting c)
            Nothing -> lift Nothing
