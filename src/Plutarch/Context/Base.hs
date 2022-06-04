{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Context.Base (
    Builder (..),
    BaseBuilder (..),
    UTXO (..),
    signedWith,
    mint,
    extraData,
    inputFromPubKey,
    outputToPubKey,
    inputFromPubKeyWith,
    outputToPubKeyWith,
    inputFromScript,
    inputFromScriptWith,
    outputToScript,
    outputToScriptWith,
    baseTxInfo,
    yieldMint,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldOutDatums,
    pubUTXOGeneral,
    pubUTXOGeneralWith,
    scriptUTXOGeneral,
    scriptUTXOGeneralWith,
) where

import Acc (Acc)
import Control.Arrow (Arrow ((&&&)))
import Control.Monad.Cont (ContT)
import Data.Foldable (Foldable (toList))
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Plutarch (S)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Context.Config (
    ContextConfig (configFee, configTimeRange, configTxId),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..), pconstant, plift)
import PlutusLedgerApi.V1.Address (
    Address,
    pubKeyHashAddress,
    scriptHashAddress,
 )
import PlutusLedgerApi.V1 (BuiltinData (BuiltinData))
import PlutusLedgerApi.V1.Contexts (
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxInfo (..),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
    ValidatorHash,
    Value,
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (Datum (Datum), DatumHash, ValidatorHash)
import PlutusLedgerApi.V1.Value (Value, symbols)
import PlutusCore.Data (Data)

data UTXOType
    = PubKeyUTXO PubKeyHash (Maybe Data)
    | ScriptUTXO ValidatorHash (Maybe Data)
    deriving stock (Show)

data UTXO = UTXO UTXOType Value
    deriving stock (Show)

utxoAddress :: UTXO -> Address
utxoAddress (UTXO typ _) = case typ of
    PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
    ScriptUTXO vh _ -> scriptHashAddress vh

utxoData :: UTXO -> Maybe (DatumHash, Datum)
utxoData (UTXO typ _) = case typ of
    PubKeyUTXO _ dat -> datumWithHash <$> dat
    ScriptUTXO _ dat -> datumWithHash <$> dat

utxoToTxOut ::
    UTXO ->
    TxOut
utxoToTxOut utxo@(UTXO _ value) =
    let address = utxoAddress utxo
        sData = utxoData utxo
     in TxOut address value . fmap fst $ sData

{- | Wraps higher level builders
 Rules:
 1. pack . upack = id
 2. unpack . pack = id
-}
class Builder a where
    pack :: BaseBuilder -> a
    unpack :: a -> BaseBuilder

data BaseBuilder = BB
    { bbInputs :: Acc UTXO
    , bbOutputs :: Acc UTXO
    , bbSignatures :: Acc PubKeyHash
    , bbDatums :: Acc Data
    , bbMints :: Acc Value
    }
    deriving stock (Show)

instance Semigroup BaseBuilder where
    BB ins outs sigs dats ms <> BB ins' outs' sigs' dats' ms' =
        BB (ins <> ins') (outs <> outs') (sigs <> sigs') (dats <> dats') (ms <> ms')

instance Monoid BaseBuilder where
    mempty = BB mempty mempty mempty mempty mempty

instance Builder BaseBuilder where
    pack = id
    unpack = id

signedWith ::
    forall (a :: Type).
    (Builder a) =>
    PubKeyHash ->
    a
signedWith pkh = pack $ mempty{bbSignatures = pure pkh}

mint ::
    forall (a :: Type).
    (Builder a) =>
    Value ->
    a
mint val = pack $ mempty{bbMints = pure val}

extraData ::
    forall (a :: Type) (d :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ d, PIsData p) =>
    d ->
    a
extraData x = pack $ mempty{bbDatums = pure . datafy $ x}

datafy ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    Data
datafy x = plift (pforgetData (pdata (pconstant x)))

inputFromPubKey ::
    forall (a :: Type).
    (Builder a) =>
    PubKeyHash ->
    Value ->
    a
inputFromPubKey pkh = pack . input . pubUTXOGeneral pkh

outputToPubKey ::
    forall (a :: Type).
    (Builder a) =>
    PubKeyHash ->
    Value ->
    a
outputToPubKey pkh = pack . output . pubUTXOGeneral pkh

inputFromPubKeyWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    a
inputFromPubKeyWith pkh val = pack . input . pubUTXOGeneralWith pkh val

outputToPubKeyWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    a
outputToPubKeyWith pkh val = pack . output . pubUTXOGeneralWith pkh val

inputFromScript ::
    forall (a :: Type).
    (Builder a) =>
    ValidatorHash ->
    Value ->
    a
inputFromScript vh = pack . input . scriptUTXOGeneral vh

inputFromScriptWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    a
inputFromScriptWith vh val = pack . input . scriptUTXOGeneralWith vh val

outputToScript ::
    forall (a :: Type).
    (Builder a) =>
    ValidatorHash ->
    Value ->
    a
outputToScript vh = pack . output . scriptUTXOGeneral vh

outputToScriptWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    a
outputToScriptWith vh val = pack . output . scriptUTXOGeneralWith vh val

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d

input ::
    UTXO ->
    BaseBuilder
input x = mempty{bbInputs = pure x}

output ::
    UTXO ->
    BaseBuilder
output x = mempty{bbOutputs = pure x}

pubUTXOGeneral :: PubKeyHash -> Value -> UTXO
pubUTXOGeneral pkh = UTXO (PubKeyUTXO pkh Nothing)

pubUTXOGeneralWith ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    UTXO
pubUTXOGeneralWith pkh val x =
    UTXO (PubKeyUTXO pkh . Just . datafy $ x) val

scriptUTXOGeneral :: ValidatorHash -> Value -> UTXO
scriptUTXOGeneral vh = UTXO (ScriptUTXO vh Nothing)

scriptUTXOGeneralWith ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    UTXO
scriptUTXOGeneralWith vh val x =
    UTXO (ScriptUTXO vh . Just . datafy $ x) val

baseTxInfo :: ContextConfig -> ContT ScriptContext Maybe TxInfo
baseTxInfo config =
    return $
        TxInfo
            { txInfoInputs = mempty
            , txInfoOutputs = mempty
            , txInfoFee = configFee config
            , txInfoMint = mempty
            , txInfoDCert = mempty
            , txInfoWdrl = mempty
            , txInfoValidRange = configTimeRange config
            , txInfoSignatories = mempty
            , txInfoData = mempty
            , txInfoId = TxId "testTx"
            }

yieldMint ::
    Acc Value ->
    ContT ScriptContext Maybe Value
yieldMint (toList -> vals) =
    return $ mconcat vals

yieldExtraDatums ::
    Acc Data ->
    ContT ScriptContext Maybe [(DatumHash, Datum)]
yieldExtraDatums (toList -> ds) =
    return $ datumWithHash <$> ds

yieldInInfoDatums ::
    Acc UTXO ->
    ContextConfig ->
    ContT ScriptContext Maybe ([TxInInfo], [(DatumHash, Datum)])
yieldInInfoDatums (toList -> inputs) config =
    return $ createTxInInfo &&& createDatumPairs $ inputs
  where
    createTxInInfo :: [UTXO] -> [TxInInfo]
    createTxInInfo xs =
        let refs = TxOutRef (configTxId config) <$> [1 ..]
         in zipWith TxInInfo refs $ utxoToTxOut <$> xs
    createDatumPairs :: [UTXO] -> [(DatumHash, Datum)]
    createDatumPairs xs = catMaybes $ utxoData <$> xs

yieldOutDatums ::
    Acc UTXO ->
    ContT ScriptContext Maybe ([TxOut], [(DatumHash, Datum)])
yieldOutDatums (toList -> outputs) =
    return $ createTxInInfo &&& createDatumPairs $ outputs
  where
    createTxInInfo :: [UTXO] -> [TxOut]
    createTxInInfo xs = utxoToTxOut <$> xs
    createDatumPairs :: [UTXO] -> [(DatumHash, Datum)]
    createDatumPairs xs = catMaybes $ utxoData <$> xs
