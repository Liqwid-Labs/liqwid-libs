{-# LANGUAGE TypeFamilies #-}

module Plutarch.Context.Base (
    UTXOType (..),
    SideUTXO (..),
    BaseBuilder (..),
    signedWith,
    mint,
    extraData,
    compileBaseTxInfo,
    inputFromPubKey,
    inputFromPubKeyWith,
    inputFromOtherScript,
    outputToPubKey,
    outputToPubKeyWith,
    outputToOtherScript,
    input,
    output,
    datafy,
    datumWithHash,
) where

import Acc (Acc)
import qualified Acc
import Control.Monad (foldM, guard)
import Data.Foldable (toList, traverse_)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Plutarch (S)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Context.Internal (
    TransactionConfig (
        testCurrencySymbol,
        testFee,
        testTimeRange,
        testTxId,
        testValidatorHash
    ),
 )
import Plutarch.Lift (PUnsafeLiftDecl (PLifted), pconstant, plift)
import Plutus.V1.Ledger.Address (
    Address,
    pubKeyHashAddress,
    scriptHashAddress,
 )
import Plutus.V1.Ledger.Api (BuiltinData (BuiltinData))
import Plutus.V1.Ledger.Contexts (
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxInfo (
        TxInfo,
        txInfoDCert,
        txInfoData,
        txInfoFee,
        txInfoId,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories,
        txInfoValidRange,
        txInfoWdrl
    ),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, symbols)
import PlutusCore.Data (Data)

data UTXOType
    = PubKeyUTXO PubKeyHash (Maybe Data)
    | ScriptUTXO ValidatorHash Data
    deriving stock (Show)

data SideUTXO (a :: Type) = SideUTXO UTXOType a
    deriving stock (Show)

data BaseBuilder (a :: Type) = BB
    { bbInputs :: Acc (SideUTXO Value)
    , bbOutputs :: Acc (SideUTXO Value)
    , bbSignatures :: Acc PubKeyHash
    , bbDatums :: Acc Data
    , bbMints :: Acc Value
    }
    deriving stock (Show)

instance Semigroup (BaseBuilder a) where
    BB ins outs sigs dats ms <> BB ins' outs' sigs' dats' ms' =
        BB (ins <> ins') (outs <> outs') (sigs <> sigs') (dats <> dats') (ms <> ms')

instance Monoid (BaseBuilder a) where
    mempty = BB mempty mempty mempty mempty mempty

signedWith ::
    forall (a :: Type).
    PubKeyHash ->
    BaseBuilder a
signedWith pkh = mempty{bbSignatures = pure pkh}

mint ::
    forall (a :: Type).
    Value ->
    BaseBuilder a
mint val = mempty{bbMints = pure val}

extraData ::
    forall (d :: Type) (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ d, PIsData p) =>
    d ->
    BaseBuilder a
extraData x = mempty{bbDatums = pure . datafy $ x}

compileBaseTxInfo ::
    forall (a :: Type).
    TransactionConfig ->
    BaseBuilder a ->
    Maybe TxInfo
compileBaseTxInfo conf builder = do
    value <- foldM toUsefulValue mempty . bbMints $ builder
    insList <- toList <$> (foldM checkSideAndCombine mempty . bbInputs $ builder)
    outsList <- toList <$> (foldM checkSideAndCombine mempty . bbOutputs $ builder)
    let signatures = toList . bbSignatures $ builder
    let dats = toList . fmap datumWithHash . bbDatums $ builder
    pure $
        TxInfo
            { txInfoInputs = createTxInInfos insList
            , txInfoOutputs = sideToTxOut <$> outsList
            , txInfoFee = testFee conf
            , txInfoMint = value
            , txInfoDCert = mempty
            , txInfoWdrl = mempty
            , txInfoValidRange = testTimeRange conf
            , txInfoSignatories = signatures
            , txInfoData =
                mapMaybe sideUtxoToDatum insList
                    <> mapMaybe sideUtxoToDatum outsList
                    <> dats
            , txInfoId = TxId "testTx"
            }
  where
    ourAddress :: Address
    ourAddress = scriptHashAddress . testValidatorHash $ conf
    ourSym :: CurrencySymbol
    ourSym = testCurrencySymbol conf
    toUsefulValue :: Value -> Value -> Maybe Value
    toUsefulValue acc val =
        (acc <>) <$> ((traverse_ (\sym -> guard (sym /= ourSym)) . symbols $ val) $> val)
    checkSideAndCombine ::
        Acc (SideUTXO Value) ->
        SideUTXO Value ->
        Maybe (Acc (SideUTXO Value))
    checkSideAndCombine acc side@(SideUTXO typ _) = do
        let sideAddress = case typ of
                PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
                ScriptUTXO hash _ -> scriptHashAddress hash
        Acc.cons <$> (guard (sideAddress /= ourAddress) $> side) <*> pure acc
    createTxInInfos :: [SideUTXO Value] -> [TxInInfo]
    createTxInInfos xs =
        let outRefs = TxOutRef (testTxId conf) <$> [1 ..]
         in zipWith TxInInfo outRefs . fmap sideToTxOut $ xs

input ::
    forall (a :: Type).
    SideUTXO Value ->
    BaseBuilder a
input x = mempty{bbInputs = pure x}

output ::
    forall (a :: Type).
    SideUTXO Value ->
    BaseBuilder a
output x = mempty{bbOutputs = pure x}

datafy ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    Data
datafy x = plift (pforgetData (pdata (pconstant x)))

inputFromPubKey ::
    forall (a :: Type).
    PubKeyHash ->
    Value ->
    BaseBuilder a
inputFromPubKey pkh = input . pubSideGeneral pkh

outputToPubKey ::
    forall (a :: Type).
    PubKeyHash ->
    Value ->
    BaseBuilder a
outputToPubKey pkh = output . pubSideGeneral pkh

inputFromPubKeyWith ::
    forall (b :: Type) (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    BaseBuilder a
inputFromPubKeyWith pkh val = input . pubSideGeneralWith pkh val

outputToPubKeyWith ::
    forall (b :: Type) (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    BaseBuilder a
outputToPubKeyWith pkh val = output . pubSideGeneralWith pkh val

inputFromOtherScript ::
    forall (b :: Type) (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    BaseBuilder a
inputFromOtherScript vh val = input . scriptSideGeneral vh val

outputToOtherScript ::
    forall (b :: Type) (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    BaseBuilder a
outputToOtherScript vh val = output . scriptSideGeneral vh val

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d

-- Helpers

dataToDatum :: Data -> Datum
dataToDatum = Datum . BuiltinData

pubSideGeneral :: PubKeyHash -> Value -> SideUTXO Value
pubSideGeneral pkh = SideUTXO (PubKeyUTXO pkh Nothing)

pubSideGeneralWith ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    SideUTXO Value
pubSideGeneralWith pkh val x =
    SideUTXO (PubKeyUTXO pkh . Just . datafy $ x) val

scriptSideGeneral ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    SideUTXO Value
scriptSideGeneral vh val x =
    SideUTXO (ScriptUTXO vh . datafy $ x) val

sideToTxOut :: SideUTXO Value -> TxOut
sideToTxOut (SideUTXO typ value) = case typ of
    PubKeyUTXO pkh mDat ->
        TxOut (pubKeyHashAddress pkh) value $ datumHash . dataToDatum <$> mDat
    ScriptUTXO hash dat ->
        TxOut (scriptHashAddress hash) value . Just . datumHash . dataToDatum $ dat

sideUtxoToDatum :: SideUTXO Value -> Maybe (DatumHash, Datum)
sideUtxoToDatum (SideUTXO typ _) = case typ of
    ScriptUTXO _ dat -> Just . datumWithHash $ dat
    PubKeyUTXO _ dat -> datumWithHash <$> dat
