{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Base
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Seungheon Oh <seungheon.ooh@gmail.com>
 Portability: GHC only
 Stability: Experimental

 Fundamental interfaces for all other higher-level context-builders.
 Such specific builders are abstracted from 'BaseBuilder'. All
 interfaces here return instances of the 'Builder' typeclass. And as
 a result, they can be used by all other instances of 'Builder'.
-}
module Plutarch.Context.Base (
    -- * Types
    Builder (..),
    BaseBuilder (..),
    UTXO (..),

    -- * Modular constructors
    ofCredential,
    ofPubKey,
    ofScript,
    to,
    outputOf,
    with,
    from,
    amount,
    refIndex,

    -- * Others
    signedWith,
    mint,
    extraData,

    -- * Builder components
    utxoToTxOut,
    yieldBaseTxInfo,
    yieldMint,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldOutDatums,
) where

import Acc (Acc)
import Control.Arrow (Arrow ((&&&)))
import Control.Monad.Cont (ContT, lift)
import Data.Foldable (Foldable (toList))
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.List (nub)
import Plutarch (S)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Context.Config (
    ContextConfig (configFee, configTimeRange, configTxId),
 )
import Plutarch.Lift (PUnsafeLiftDecl (..), pconstant, plift)
import PlutusCore.Data (Data)
import PlutusLedgerApi.V1 (
    BuiltinData (BuiltinData),
    Credential (..),
 )
import PlutusLedgerApi.V1.Address (
    Address,
    pubKeyHashAddress,
    scriptHashAddress,
 )
import PlutusLedgerApi.V1.Contexts (
    ScriptContext (..),
    TxId (..),
    TxInInfo (TxInInfo),
    TxInfo (..),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Scripts (
    Datum (Datum),
    DatumHash,
    ValidatorHash,
 )
import PlutusLedgerApi.V1.Value (Value)

{- | 'UTXO' is used to represent any input or output of a transaction in the builders.

A UTXO contains:

  - A `Value` of one or more assets.
  - An `Address` that it exists at.
  - Potentially some Plutus 'Data'.

This is different from `TxOut`, in that we store the 'Data' fully instead of the 'DatumHash'.

 @since 1.1.0
-}
data UTXO = UTXO
    { utxoCredential :: Credential
    , utxoValue :: Value
    , utxoData :: Maybe Data
    , utxoTxId :: Maybe TxId
    , utxoTxIdx :: Maybe Integer
    }
    deriving stock (Show)

{- | Pulls address output of given UTXO

 @since 1.1.0
-}
utxoAddress :: UTXO -> Address
utxoAddress UTXO{..} = case utxoCredential of
    PubKeyCredential pkh -> pubKeyHashAddress pkh
    ScriptCredential vh -> scriptHashAddress vh

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d    

{- | Construct DatumHash-Datum pair of given UTXO

 @since 1.1.0
-}
utxoDatumPair :: UTXO -> Maybe (DatumHash, Datum)
utxoDatumPair UTXO{..} = datumWithHash <$> utxoData

{- | Construct TxOut of given UTXO

 @since 1.1.0
-}
utxoToTxOut ::
    UTXO ->
    TxOut
utxoToTxOut utxo@(UTXO{..}) =
    let address = utxoAddress utxo
        sData = utxoDatumPair utxo
     in TxOut address utxoValue . fmap fst $ sData

{- | An abstraction for the higher-level builders.
 It allows those builder to integrate base builder functionalities
 without any duplications.

 @unpack@ will sometimes loose data.

 Typeclass Rules:
 1. unpack . pack = id

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

with ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    b ->
    UTXO ->
    UTXO
with dat u = u{utxoData = Just . datafy $ dat}

outputOf :: TxId -> UTXO -> UTXO
outputOf tid u = u{utxoTxId = Just tid}

refIndex :: Integer -> UTXO -> UTXO
refIndex tidx u = u{utxoTxIdx = Just tidx}

amount :: Value -> UTXO -> UTXO
amount val u = u{utxoValue = val}

ofCredential :: Credential -> UTXO -> UTXO
ofCredential cred u = u{utxoCredential = cred}

ofPubKey :: PubKeyHash -> UTXO -> UTXO
ofPubKey (PubKeyCredential -> cred) u = u{utxoCredential = cred}

ofScript :: ValidatorHash -> UTXO -> UTXO
ofScript (ScriptCredential -> cred) u = u{utxoCredential = cred}

to ::
    forall (a :: Type).
    (Builder a) =>
    (UTXO -> UTXO) ->
    a
to f = pack . output . f $ UTXO (PubKeyCredential "") mempty Nothing Nothing Nothing

from ::
    forall (a :: Type).
    (Builder a) =>
    (UTXO -> UTXO) ->
    a
from f = pack . input . f $ UTXO (PubKeyCredential "") mempty Nothing Nothing Nothing

input ::
    UTXO ->
    BaseBuilder
input x = mempty{bbInputs = pure x}

output ::
    UTXO ->
    BaseBuilder
output x = mempty{bbOutputs = pure x}

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
            , txInfoId = configTxId config
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
yieldInInfoDatums (toList -> inputs) config
  | length (nub takenIdx) /= length takenIdx = lift Nothing
  | otherwise = return $ createTxInInfo &&& createDatumPairs $ inputs
  where
    createTxInInfo :: [UTXO] -> [TxInInfo]
    createTxInInfo = mkTxInInfo 1

    takenIdx = catMaybes $ utxoTxIdx <$> inputs
    
    mkTxInInfo :: Integer -> [UTXO] -> [TxInInfo]
    mkTxInInfo _ [] = []
    mkTxInInfo ind (utxo@(UTXO{..}): xs)
      | elem ind takenIdx = mkTxInInfo (ind + 1) $ utxo:xs
      | otherwise = case utxoTxIdx of
          Just x -> TxInInfo (ref x) (utxoToTxOut utxo) : mkTxInInfo (ind) xs
          Nothing -> TxInInfo (ref ind) (utxoToTxOut utxo) : mkTxInInfo (ind + 1) xs
      where
        ref = TxOutRef (maybe (configTxId config) id utxoTxId)
            
    createDatumPairs :: [UTXO] -> [(DatumHash, Datum)]
    createDatumPairs xs = catMaybes $ utxoDatumPair <$> xs

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
    createDatumPairs xs = catMaybes $ utxoDatumPair <$> xs
