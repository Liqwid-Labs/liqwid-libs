{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutarch.Context.Base
 Copyright: (C) Liqwid Labs 2022
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
    output,
    input,
    referenceInput,
    credential,
    pubKey,
    script,
    withRefTxId,
    withDatum,
    withInlineDatum,
    withValue,
    withRefIndex,
    withRef,

    -- * Others
    unpack,
    signedWith,
    mint,
    extraData,
    txId,
    fee,
    timeRange,

    -- * Builder components
    utxoToTxOut,
    datumWithHash,
    utxoDatumPair,
    yieldBaseTxInfo,
    yieldMint,
    yieldExtraDatums,
    yieldInInfoDatums,
    yieldOutDatums,
    mkOutRefIndices,
) where

import Acc (Acc)
import Control.Arrow (Arrow ((&&&)))
import Data.Foldable (Foldable (toList))
import Data.Kind (Type)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Exts (fromList)
import Optics (Lens', lens, over, view)
import Plutarch (S)
import Plutarch.Api.V2 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Lift (PUnsafeLiftDecl (..), pconstant, plift)
import PlutusLedgerApi.V2 (
    Address (Address),
    BuiltinData (BuiltinData),
    Credential (..),
    Data,
    Datum (Datum),
    DatumHash,
    Interval,
    OutputDatum (..),
    POSIXTime,
    PubKeyHash (PubKeyHash),
    ScriptHash,
    TxId (TxId),
    TxInInfo (TxInInfo),
    TxInfo (..),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
    ValidatorHash,
    Value,
    always,
 )
import qualified PlutusTx.AssocMap as AssocMap (fromList)

-- | @since 2.1.0
data DatumType
    = InlineDatum Data
    | ContextDatum Data
    deriving stock (Show)

{- | 'UTXO' is used to represent any input or output of a transaction in the builders.

A UTXO' contains:

  - A `Value` of one or more assets.
  - An `Address` that it exists at.
  - Potentially some Plutus 'Data'.

This is different from `TxOut`, in that we store the 'Data' fully instead of the 'DatumHash'.

 @since 2.0.0
-}
data UTXO = UTXO
    { utxoCredential :: Maybe Credential
    , utxoValue :: Value
    , utxoData :: Maybe DatumType
    , utxoReferenceScript :: Maybe ScriptHash
    , utxoTxId :: Maybe TxId
    , utxoTxIdx :: Maybe Integer
    }
    deriving stock (Show)

-- | @since 2.0.0
instance Semigroup UTXO where
    (UTXO c v d rs t tidx) <> (UTXO c' v' d' rs' t' tidx') =
        UTXO (choose c c') (v <> v') (choose d d') (choose rs rs') (choose t t') (choose tidx tidx')
      where
        choose _ (Just b) = Just b
        choose a Nothing = a

-- | @since 2.0.0
instance Monoid UTXO where
    mempty = UTXO Nothing mempty Nothing Nothing Nothing Nothing

{- | Pulls address output of given UTXO'

 @since 1.1.0
-}
utxoAddress :: UTXO -> Address
utxoAddress UTXO{..} = case utxoCredential of
    Just cred -> Address cred Nothing
    Nothing -> Address (PubKeyCredential $ PubKeyHash "") Nothing

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d

utxoOutputDatum :: UTXO -> OutputDatum
utxoOutputDatum (utxoData -> d') = case d' of
    Nothing -> NoOutputDatum
    Just (InlineDatum d) -> OutputDatum (Datum . BuiltinData $ d)
    Just (ContextDatum d) -> OutputDatumHash (datumHash . Datum . BuiltinData $ d)

utxoDatumPair :: UTXO -> Maybe (DatumHash, Datum)
utxoDatumPair u =
    utxoData u >>= \case
        InlineDatum _ -> Nothing
        ContextDatum d -> Just $ datumWithHash d

{- | Construct TxOut of given UTXO

 @since 1.1.0
-}
utxoToTxOut ::
    UTXO ->
    TxOut
utxoToTxOut utxo@(UTXO{..}) =
    TxOut (utxoAddress utxo) utxoValue (utxoOutputDatum utxo) utxoReferenceScript

withDatum ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    b ->
    UTXO
withDatum dat = mempty{utxoData = Just . ContextDatum . datafy $ dat}

withInlineDatum ::
    forall (b :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
    b ->
    UTXO
withInlineDatum dat = mempty{utxoData = Just . InlineDatum . datafy $ dat}

withRefTxId :: TxId -> UTXO
withRefTxId tid = mempty{utxoTxId = Just tid}

withRefIndex :: Integer -> UTXO
withRefIndex tidx = mempty{utxoTxIdx = Just tidx}

withRef :: TxOutRef -> UTXO
withRef (TxOutRef tid idx) = withRefTxId tid <> withRefIndex idx

withValue :: Value -> UTXO
withValue val = mempty{utxoValue = val}

credential :: Credential -> UTXO
credential cred = mempty{utxoCredential = Just cred}

pubKey :: PubKeyHash -> UTXO
pubKey (PubKeyCredential -> cred) = mempty{utxoCredential = Just cred}

script :: ValidatorHash -> UTXO
script (ScriptCredential -> cred) = mempty{utxoCredential = Just cred}

{- | Interface between specific builders to BaseBuilder.
 @pack@ will constrct specific builder that contains given BaseBuilder.

 Laws:
 1. view _bb . pack = id
 2. pack . view _bb = id

 @since 1.1.0
-}
class Builder a where
    _bb :: Lens' a BaseBuilder
    pack :: BaseBuilder -> a

-- | @since 2.1.0
unpack :: Builder a => a -> BaseBuilder
unpack = view _bb

{- | Base builder. Handles basic input, output, signs, mints, and
 extra datums. BaseBuilder provides such basic functionalities for
 @ScriptContext@ creation, leaving specific builders only with
 minimal logical checkings.

 @since 1.1.0
-}
data BaseBuilder = BB
    { bbInputs :: Acc UTXO
    , bbReferenceInputs :: Acc UTXO
    , bbOutputs :: Acc UTXO
    , bbSignatures :: Acc PubKeyHash
    , bbDatums :: Acc Data
    , bbMints :: Acc Value
    , bbFee :: Value
    , bbTimeRange :: Interval POSIXTime
    , bbTxId :: TxId
    }
    deriving stock (Show)

-- | @since 1.1.0
instance Semigroup BaseBuilder where
    BB ins refIns outs sigs dats ms fval range tid <> BB ins' refIns' outs' sigs' dats' ms' fval' range' tid' =
        BB
            (ins <> ins')
            (refIns <> refIns')
            (outs <> outs')
            (sigs <> sigs')
            (dats <> dats')
            (ms <> ms')
            (fval <> fval')
            (choose bbTimeRange range range')
            (choose bbTxId tid tid')
      where
        choose f a b
            | f mempty == b = a
            | otherwise = b

-- | @since 1.1.0
instance Monoid BaseBuilder where
    mempty = BB mempty mempty mempty mempty mempty mempty mempty always (TxId "")

-- | @since 2.1.0
instance Builder BaseBuilder where
    _bb = lens id $ const id
    pack = id

datafy ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    Data
datafy x = plift (pforgetData (pdata (pconstant x)))

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

txId ::
    forall (a :: Type).
    (Builder a) =>
    TxId ->
    a
txId tid = pack $ mempty{bbTxId = tid}

fee ::
    forall (a :: Type).
    (Builder a) =>
    Value ->
    a
fee val = pack $ mempty{bbFee = val}

timeRange ::
    forall (a :: Type).
    (Builder a) =>
    Interval POSIXTime ->
    a
timeRange r = pack $ mempty{bbTimeRange = r}

output ::
    forall (a :: Type).
    (Builder a) =>
    UTXO ->
    a
output x = pack mempty{bbOutputs = pure x}

input ::
    forall (a :: Type).
    (Builder a) =>
    UTXO ->
    a
input x = pack mempty{bbInputs = pure x}

referenceInput ::
    forall (a :: Type).
    (Builder a) =>
    UTXO ->
    a
referenceInput x = pack mempty{bbReferenceInputs = pure x}

{- | Provide base @TxInfo@ to Continuation Monad.

 @since 1.1.0
-}
yieldBaseTxInfo ::
    (Builder b) => b -> TxInfo
yieldBaseTxInfo (unpack -> BB{..}) =
    TxInfo
        { txInfoInputs = mempty
        , txInfoReferenceInputs = mempty
        , txInfoOutputs = mempty
        , txInfoFee = bbFee
        , txInfoMint = mempty
        , txInfoDCert = mempty
        , txInfoWdrl = AssocMap.fromList []
        , txInfoValidRange = bbTimeRange
        , txInfoSignatories = mempty
        , txInfoRedeemers = AssocMap.fromList []
        , txInfoData = AssocMap.fromList []
        , txInfoId = bbTxId
        }

{- | Provide total mints to Continuation Monad.

 @since 1.1.0
-}
yieldMint ::
    Acc Value ->
    Value
yieldMint (toList -> vals) =
    mconcat vals

{- | Provide DatumHash-Datum pair to Continuation Monad.

 @since 1.1.0
-}
yieldExtraDatums ::
    Acc Data ->
    [(DatumHash, Datum)]
yieldExtraDatums (toList -> ds) =
    datumWithHash <$> ds

{- | Provide list of TxInInfo and DatumHash-Datum pair for inputs to
 Continutation Monad.

 @since 1.1.0
-}
yieldInInfoDatums ::
    Acc UTXO ->
    ([TxInInfo], [(DatumHash, Datum)])
yieldInInfoDatums (toList -> inputs) =
    fmap mkTxInInfo &&& createDatumPairs $ inputs
  where
    mkTxInInfo :: UTXO -> TxInInfo
    mkTxInInfo utxo@UTXO{..} = TxInInfo ref (utxoToTxOut utxo)
      where
        ref = TxOutRef (fromMaybe "" utxoTxId) (fromMaybe 0 utxoTxIdx)

    createDatumPairs :: [UTXO] -> [(DatumHash, Datum)]
    createDatumPairs = mapMaybe utxoDatumPair

{- | Provide list of TxOut and DatumHash-Datum pair for outputs to
 Continutation Monad.

 @since 1.1.0
-}
yieldOutDatums ::
    Acc UTXO ->
    ([TxOut], [(DatumHash, Datum)])
yieldOutDatums (toList -> outputs) =
    createTxInInfo &&& createDatumPairs $ outputs
  where
    createTxInInfo :: [UTXO] -> [TxOut]
    createTxInInfo xs = utxoToTxOut <$> xs
    createDatumPairs :: [UTXO] -> [(DatumHash, Datum)]
    createDatumPairs = mapMaybe utxoDatumPair

{- | Automatically generate `TxOutRef`s from given builder.
 It tries to reserve index if given one; otherwise, it grants
 incremental indices to each inputs starting from one. If there
 are duplicate reserved indices, the second occurance will be
 treated as non-reserved and given incremental index.

 @since 2.1.0
-}
mkOutRefIndices ::
    Builder a =>
    a ->
    a
mkOutRefIndices = over _bb go
  where
    go bb@BB{..} = bb{bbInputs = grantIndex bbInputs}

    grantIndex :: Acc UTXO -> Acc UTXO
    grantIndex (toList -> xs) = fromList $ grantIndex' 1 [] xs

    grantIndex' :: Integer -> [Integer] -> [UTXO] -> [UTXO]
    grantIndex' top used (x@(UTXO _ _ _ _ _ Nothing) : xs)
        | top `elem` used = grantIndex' (top + 1) used (x : xs)
        | otherwise = x{utxoTxIdx = Just top} : grantIndex' (top + 1) (top : used) xs
    grantIndex' top used (x@(UTXO _ _ _ _ _ (Just i)) : xs)
        | i `elem` used = grantIndex' top used (x{utxoTxIdx = Nothing} : xs)
        | otherwise = x : grantIndex' top (i : used) xs
    grantIndex' _ _ [] = []
