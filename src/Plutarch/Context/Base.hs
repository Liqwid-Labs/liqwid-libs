{-# LANGUAGE TypeFamilies #-}

module Plutarch.Context.Base (
    BaseBuilder (..),
    signedWith,
    mint,
    extraData,
    compileBaseTxInfo,
    input,
    output,
    datafy,
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
    SideUTXO (SideUTXO),
    TransactionConfig (
        testCurrencySymbol,
        testFee,
        testTimeRange,
        testTxId,
        testValidatorHash
    ),
    UTXOType (PubKeyUTXO, ScriptUTXO),
    ValueType (GeneralValue, TokensValue),
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
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, symbols)
import qualified Plutus.V1.Ledger.Value as Value
import PlutusCore.Data (Data)

data BaseBuilder (a :: Type) = BB
    { bbInputs :: Acc SideUTXO
    , bbOutputs :: Acc SideUTXO
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
            , txInfoOutputs = sideToTxOut ourSym <$> outsList
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
    checkSideAndCombine :: Acc SideUTXO -> SideUTXO -> Maybe (Acc SideUTXO)
    checkSideAndCombine acc side@(SideUTXO typ _) = do
        let sideAddress = case typ of
                PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
                ScriptUTXO hash _ -> scriptHashAddress hash
        Acc.cons <$> (guard (sideAddress /= ourAddress) $> side) <*> pure acc
    createTxInInfos :: [SideUTXO] -> [TxInInfo]
    createTxInInfos xs =
        let outRefs = TxOutRef (testTxId conf) <$> [1 ..]
         in zipWith TxInInfo outRefs . fmap (sideToTxOut ourSym) $ xs

input ::
    forall (a :: Type).
    SideUTXO ->
    BaseBuilder a
input x = mempty{bbInputs = pure x}

output ::
    forall (a :: Type).
    SideUTXO ->
    BaseBuilder a
output x = mempty{bbOutputs = pure x}

datafy ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    Data
datafy x = plift (pforgetData (pdata (pconstant x)))

-- Helpers

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d

sideToTxOut :: CurrencySymbol -> SideUTXO -> TxOut
sideToTxOut sym (SideUTXO typ val) =
    let value = extractValue sym val
     in case typ of
            PubKeyUTXO pkh mDat ->
                TxOut (pubKeyHashAddress pkh) value $ datumHash . dataToDatum <$> mDat
            ScriptUTXO hash dat ->
                TxOut (scriptHashAddress hash) value . Just . datumHash . dataToDatum $ dat

sideUtxoToDatum :: SideUTXO -> Maybe (DatumHash, Datum)
sideUtxoToDatum (SideUTXO typ _) = case typ of
    ScriptUTXO _ dat -> Just . datumWithHash $ dat
    PubKeyUTXO _ dat -> datumWithHash <$> dat

extractValue :: CurrencySymbol -> ValueType -> Value
extractValue sym = \case
    GeneralValue val -> val
    TokensValue name amount -> Value.singleton sym name amount

dataToDatum :: Data -> Datum
dataToDatum = Datum . BuiltinData
