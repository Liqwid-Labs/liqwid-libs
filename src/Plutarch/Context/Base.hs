{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Base
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Seungheon Oh <seungheon.ooh@gmail.com>
 Portability: GHC only
 Stability: Experimental

 Fundumental interfaces for all other higher-level builders. Those
 specific builders are abstracted from the @BaseBuilder@. All
 interfaces here returns an instances of @Builder@ typeclass, and,
 thus, can be used by all other builders.
-}
module Plutarch.Context.Base (
    -- * Types
    Builder (..),
    BaseBuilder (..),
    UTXO (..),

    -- * Inputs
    inputFromPubKey,
    inputFromPubKeyWith,
    inputFromScript,
    inputFromScriptWith,

    -- * Outputs
    outputToPubKey,
    outputToPubKeyWith,
    outputToScript,
    outputToScriptWith,

    -- * Others
    signedWith,
    mint,
    extraData,

    -- * Builder components
    yieldBaseTxInfo,
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
import PlutusCore.Data (Data)
import PlutusLedgerApi.V1 (BuiltinData (BuiltinData))
import PlutusLedgerApi.V1.Address (
    Address,
    pubKeyHashAddress,
    scriptHashAddress,
 )
import PlutusLedgerApi.V1.Contexts (
    ScriptContext (..),
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxInfo (..),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (Datum (Datum), DatumHash, ValidatorHash)
import PlutusLedgerApi.V1.Value (Value)

{- | Possible Unspend Transaction Outputs type.
 There are only two different types: outputs from PubKeyHash or
 ValidatorHash. Both may or may not have datum attached.

 @since 1.0.0
-}
data UTXOType
    = PubKeyUTXO PubKeyHash (Maybe Data)
    | ScriptUTXO ValidatorHash (Maybe Data)
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | UTXO representation. Any input or output of a transaction will have
 this UTXO structure to be represented in the builders.

 @since 1.1.0
-}
data UTXO = UTXO UTXOType Value
    deriving stock (Show)

{- | Pulls address output of given UTXO

 @since 1.1.0
-}
utxoAddress :: UTXO -> Address
utxoAddress (UTXO typ _) = case typ of
    PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
    ScriptUTXO vh _ -> scriptHashAddress vh

{- | Construct DatumHash-Datum pair of given UTXO

 @since 1.1.0
-}
utxoData :: UTXO -> Maybe (DatumHash, Datum)
utxoData (UTXO typ _) = case typ of
    PubKeyUTXO _ dat -> datumWithHash <$> dat
    ScriptUTXO _ dat -> datumWithHash <$> dat

{- | Construct TxOut of given UTXO

 @since 1.1.0
-}
utxoToTxOut ::
    UTXO ->
    TxOut
utxoToTxOut utxo@(UTXO _ value) =
    let address = utxoAddress utxo
        sData = utxoData utxo
     in TxOut address value . fmap fst $ sData

{- | An abstraction for the higher-level builders.
 It allows those builder to integrate base builder functionalities
 without any duplications.

 @unpack@ will sometimes loose data.

 Typeclass Rules:
 1. pack . upack = id

 @since 1.1.0
-}
class Monoid a => Builder a where
    pack :: BaseBuilder -> a
    unpack :: a -> BaseBuilder

{- | Base builder. Handles basic input, output, signs, mints, and
 extra datums. BaseBuilder provides such basic functionalities for
 @ScriptContext@ creation, leaving specific builders only with
 minimal logical checkings.

 @since 1.1.0
-}
data BaseBuilder = BB
    { bbInputs :: Acc UTXO
    , bbOutputs :: Acc UTXO
    , bbSignatures :: Acc PubKeyHash
    , bbDatums :: Acc Data
    , bbMints :: Acc Value
    }
    deriving stock (Show)

-- | @since 1.1.0
instance Semigroup BaseBuilder where
    BB ins outs sigs dats ms <> BB ins' outs' sigs' dats' ms' =
        BB (ins <> ins') (outs <> outs') (sigs <> sigs') (dats <> dats') (ms <> ms')

-- | @since 1.1.0
instance Monoid BaseBuilder where
    mempty = BB mempty mempty mempty mempty mempty

-- | @since 1.1.0
instance Builder BaseBuilder where
    pack = id
    unpack = id

{- | Adds signer to builder.

 @since 1.1.0
-}
signedWith ::
    forall (a :: Type).
    (Builder a) =>
    PubKeyHash ->
    a
signedWith pkh = pack $ mempty{bbSignatures = pure pkh}

{- | Mint given value.

 @since 1.1.0
-}
mint ::
    forall (a :: Type).
    (Builder a) =>
    Value ->
    a
mint val = pack $ mempty{bbMints = pure val}

{- | Append extra datum to @ScriptContex@.

 @since 1.1.0
-}
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

{- | Add input UTXO from PubKey.

 @since 1.1.0
-}
inputFromPubKey ::
    forall (a :: Type).
    (Builder a) =>
    PubKeyHash ->
    Value ->
    a
inputFromPubKey pkh = pack . input . pubUTXOGeneral pkh

{- | Add output UTXO to PubKey.

 @since 1.1.0
-}
outputToPubKey ::
    forall (a :: Type).
    (Builder a) =>
    PubKeyHash ->
    Value ->
    a
outputToPubKey pkh = pack . output . pubUTXOGeneral pkh

{- | Add input UTXO from PubKey with Datum.

 @since 1.1.0
-}
inputFromPubKeyWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    a
inputFromPubKeyWith pkh val = pack . input . pubUTXOGeneralWith pkh val

{- | Add output UTXO to PubKey with Datum.

 @since 1.1.0
-}
outputToPubKeyWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    a
outputToPubKeyWith pkh val = pack . output . pubUTXOGeneralWith pkh val

{- | Add input UTXO from ValidatorHash.

 @since 1.1.0
-}
inputFromScript ::
    forall (a :: Type).
    (Builder a) =>
    ValidatorHash ->
    Value ->
    a
inputFromScript vh = pack . input . scriptUTXOGeneral vh

{- | Add input UTXO from ValidatorHash with Datum.

 @since 1.1.0
-}
inputFromScriptWith ::
    forall (a :: Type) (b :: Type) (p :: S -> Type).
    (Builder a, PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    a
inputFromScriptWith vh val = pack . input . scriptUTXOGeneralWith vh val

{- | Add output UTXO to ValidatorHash.

 @since 1.1.0
-}
outputToScript ::
    forall (a :: Type).
    (Builder a) =>
    ValidatorHash ->
    Value ->
    a
outputToScript vh = pack . output . scriptUTXOGeneral vh

{- | Add output UTXO to ValidatorHash with Datum.

 @since 1.1.0
-}
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

{- | Construct @UTXO@ from PubKeyHash.

 @since 1.1.0
-}
pubUTXOGeneral :: PubKeyHash -> Value -> UTXO
pubUTXOGeneral pkh = UTXO (PubKeyUTXO pkh Nothing)

{- | Construct @UTXO@ from PubKeyHash with Datum.

 @since 1.1.0
-}
pubUTXOGeneralWith ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    PubKeyHash ->
    Value ->
    b ->
    UTXO
pubUTXOGeneralWith pkh val x =
    UTXO (PubKeyUTXO pkh . Just . datafy $ x) val

{- | Construct @UTXO@ from ValidatorHash.

 @since 1.1.0
-}
scriptUTXOGeneral :: ValidatorHash -> Value -> UTXO
scriptUTXOGeneral vh = UTXO (ScriptUTXO vh Nothing)

{- | Construct @UTXO@ from ValidatorHash with Datum.

 @since 1.1.0
-}
scriptUTXOGeneralWith ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    ValidatorHash ->
    Value ->
    b ->
    UTXO
scriptUTXOGeneralWith vh val x =
    UTXO (ScriptUTXO vh . Just . datafy $ x) val

{- | Provide base @TxInfo@ to Continuation Monad.

 @since 1.1.0
-}
yieldBaseTxInfo :: ContextConfig -> ContT ScriptContext Maybe TxInfo
yieldBaseTxInfo config =
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

{- | Provide total mints to Continuation Monad.

 @since 1.1.0
-}
yieldMint ::
    Acc Value ->
    ContT ScriptContext Maybe Value
yieldMint (toList -> vals) =
    return $ mconcat vals

{- | Provide DatumHash-Datum pair to Continuation Monad.

 @since 1.1.0
-}
yieldExtraDatums ::
    Acc Data ->
    ContT ScriptContext Maybe [(DatumHash, Datum)]
yieldExtraDatums (toList -> ds) =
    return $ datumWithHash <$> ds

{- | Provide list of TxInInfo and DatumHash-Datum pair for inputs to
 Continutation Monad.

 @since 1.1.0
-}
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

{- | Provide list of TxOut and DatumHash-Datum pair for outputs to
 Continutation Monad.

 @since 1.1.0
-}
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
