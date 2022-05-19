{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Context.Base (
    UTXOType (..),
    SideUTXO (..),
    sideAddress,
    sideData,
    sideToTxOut,
    BaseBuilder (..),
    Build,
    runBuild,
    freshOutRefIndex,
    baseTxInfo,
    toInInfoDatums,
    toTxOutDatums,
    combineExternalMints,
    signedWith,
    mint,
    extraData,
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
import Control.Applicative (Alternative)
import Control.Monad (foldM, guard)
import Control.Monad.RWS.Strict (RWST, evalRWST)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (get, modify)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Plutarch (S)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Context.Config (
    ContextConfig (
        configCurrencySymbol,
        configFee,
        configTimeRange,
        configTxId,
        configValidatorHash
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

sideAddress :: forall (a :: Type). SideUTXO a -> Address
sideAddress (SideUTXO typ _) = case typ of
    PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
    ScriptUTXO vh _ -> scriptHashAddress vh

sideData :: forall (a :: Type). SideUTXO a -> Maybe (DatumHash, Datum)
sideData (SideUTXO typ _) = case typ of
    PubKeyUTXO _ dat -> datumWithHash <$> dat
    ScriptUTXO _ dat -> Just . datumWithHash $ dat

sideToTxOut ::
    forall (a :: Type).
    (a -> Value) ->
    SideUTXO a ->
    TxOut
sideToTxOut f side@(SideUTXO _ x) =
    let address = sideAddress side
        sData = sideData side
        value = f x
     in TxOut address value . fmap fst $ sData

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

-- Build monad and helpers

newtype Build (a :: Type) = Build (RWST ContextConfig () Integer Maybe a)
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadReader ContextConfig
        )
        via (RWST ContextConfig () Integer Maybe)

runBuild ::
    forall (a :: Type).
    Build a ->
    ContextConfig ->
    Maybe a
runBuild (Build comp) config = fst <$> evalRWST comp config 1

freshOutRefIndex :: Build Integer
freshOutRefIndex = Build $ do
    curr <- get
    modify (+ 1)
    pure curr

baseTxInfo :: Build TxInfo
baseTxInfo = do
    fee <- asks configFee
    time <- asks configTimeRange
    pure $
        TxInfo
            { txInfoInputs = mempty
            , txInfoOutputs = mempty
            , txInfoFee = fee
            , txInfoMint = mempty
            , txInfoDCert = mempty
            , txInfoWdrl = mempty
            , txInfoValidRange = time
            , txInfoSignatories = mempty
            , txInfoData = mempty
            , txInfoId = TxId "testTx"
            }

toInInfoDatums ::
    forall (f :: Type -> Type) (a :: Type).
    (Foldable f, Applicative f, forall b. Monoid (f b)) =>
    (a -> Value) ->
    f (SideUTXO a) ->
    Build (f TxInInfo, f (DatumHash, Datum))
toInInfoDatums proj = foldM go (mempty, mempty)
  where
    go ::
        (f TxInInfo, f (DatumHash, Datum)) ->
        SideUTXO a ->
        Build (f TxInInfo, f (DatumHash, Datum))
    go (acc1, acc2) sideUtxo = do
        ourAddress <- asks (scriptHashAddress . configValidatorHash)
        guard (ourAddress /= sideAddress sideUtxo)
        ref <- asks (TxOutRef . configTxId) <*> freshOutRefIndex
        let mSideData = sideData sideUtxo
        let txOut = sideToTxOut proj sideUtxo
        let txInInfo = TxInInfo ref txOut
        pure . (pure txInInfo <> acc1,) $ case mSideData of
            Nothing -> acc2
            Just sd -> pure sd <> acc2

toTxOutDatums ::
    forall (f :: Type -> Type) (a :: Type).
    (Foldable f, Applicative f, forall b. Monoid (f b)) =>
    (a -> Value) ->
    f (SideUTXO a) ->
    (f TxOut, f (DatumHash, Datum))
toTxOutDatums proj = foldr go (mempty, mempty)
  where
    go ::
        SideUTXO a ->
        (f TxOut, f (DatumHash, Datum)) ->
        (f TxOut, f (DatumHash, Datum))
    go sideUtxo (acc1, acc2) =
        let txOut = sideToTxOut proj sideUtxo
            mSideData = sideData sideUtxo
         in (acc1 <> pure txOut,) $ case mSideData of
                Nothing -> acc2
                Just sd -> acc2 <> pure sd

combineExternalMints ::
    forall (f :: Type -> Type).
    (Foldable f) =>
    f Value ->
    Build Value
combineExternalMints = foldM go mempty
  where
    go :: Value -> Value -> Build Value
    go acc val =
        (acc <>) <$> do
            traverse_ checkSyms . symbols $ val
            pure val
    checkSyms :: CurrencySymbol -> Build ()
    checkSyms sym =
        asks configCurrencySymbol >>= (guard . (sym /=))

-- Helpers

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
