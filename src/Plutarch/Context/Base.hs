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
  Mint (..),

  -- * Modular constructors
  output,
  input,
  referenceInput,
  address,
  credential,
  pubKey,
  script,
  withStakingCredential,
  withRefTxId,
  withDatum,
  withInlineDatum,
  withReferenceScript,
  withValue,
  withRefIndex,
  withRef,
  withRedeemer,

  -- * Others
  unpack,
  signedWith,
  mint,
  mintWith,
  mintSingletonWith,
  extraData,
  extraRedeemer,
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
  yieldRedeemerMap,
  mkOutRefIndices,
  mintToValue,

  -- * Normalizers
  combinePair,
  combineMap,
  normalizeValue,
  normalizeUTXO,
  normalizeMint,
  sortMap,
  mkNormalizedBase,
) where

import Acc (Acc, fromReverseList)
import Control.Arrow (Arrow ((&&&)))
import Data.Foldable (Foldable (toList))
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import GHC.Exts (fromList)
import Optics (Lens', lens, over, view)
import Plutarch (S)
import Plutarch.Api.V2 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted), pconstant, plift)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Address (Address, addressCredential, addressStakingCredential),
  BuiltinData (BuiltinData),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  Data,
  Datum (Datum),
  DatumHash,
  Interval,
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  POSIXTime,
  PubKeyHash (PubKeyHash),
  Redeemer (Redeemer),
  ScriptHash,
  ScriptPurpose (Minting, Spending),
  StakingCredential,
  TokenName,
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
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value (getValue),
  adaSymbol,
  adaToken,
  always,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap

-- | @since 2.1.0
data DatumType
  = InlineDatum Data
  | ContextDatum Data
  deriving stock (Show)

{- | Minted tokens that have the same symbol, and the redeemer used to mint
      those tokens.

     @since 2.3.0
-}
data Mint = Mint
  { mintSymbol :: CurrencySymbol
  , mintTokens :: [(TokenName, Integer)]
  , mintRedeemer :: Data
  }
  deriving stock
    ( -- | @since 2.3.0
      Show
    , -- | @since 2.4.0
      Eq
    )

{- | Normalize mint amount.

 @since 2.4.0
-}
normalizeMint :: Mint -> Mint
normalizeMint m =
  m
    { mintTokens =
        sortBy (\(t, _) (t', _) -> compare t t') $
          filter (\(_, v) -> v /= 0) $
            foldr (combinePair (+)) [] $ mintTokens m
    }

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
  , utxoStakingCredential :: Maybe StakingCredential
  , utxoValue :: Value
  , utxoData :: Maybe DatumType
  , utxoReferenceScript :: Maybe ScriptHash
  , utxoTxId :: Maybe TxId
  , utxoTxIdx :: Maybe Integer
  , utxoRedeemer :: Maybe Data
  }
  deriving stock (Show)

-- | @since 2.0.0
instance Semigroup UTXO where
  (UTXO c stc v d rs t tidx r) <> (UTXO c' stc' v' d' rs' t' tidx' r') =
    UTXO
      (choose c c')
      (choose stc stc')
      (v <> v')
      (choose d d')
      (choose rs rs')
      (choose t t')
      (choose tidx tidx')
      (choose r r')
    where
      choose _ (Just b) = Just b
      choose a Nothing = a

-- | @since 2.0.0
instance Monoid UTXO where
  mempty = UTXO Nothing Nothing mempty Nothing Nothing Nothing Nothing Nothing

{- | Pulls address output of given UTXO'

 @since 1.1.0
-}
utxoAddress :: UTXO -> Address
utxoAddress UTXO {..} =
  let cred = fromMaybe (PubKeyCredential $ PubKeyHash "") utxoCredential
   in Address cred utxoStakingCredential

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

{- | Normalize the value 'UTXO' holds.

 @since 2.4.0
-}
normalizeUTXO :: UTXO -> UTXO
normalizeUTXO utxo =
  utxo
    { utxoValue = normalizeValue $ utxoValue utxo
    }

{- | Construct TxOut of given UTXO

 @since 1.1.0
-}
utxoToTxOut ::
  UTXO ->
  TxOut
utxoToTxOut utxo@(UTXO {..}) =
  TxOut (utxoAddress utxo) utxoValue (utxoOutputDatum utxo) utxoReferenceScript

{- | Specify datum of a UTXO.

 @since 2.0.0
-}
withDatum ::
  forall (b :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
  b ->
  UTXO
withDatum dat = mempty {utxoData = Just . ContextDatum . datafy $ dat}

{- | Specify in-line datum of a UTXO.

 @since 2.0.0
-}
withInlineDatum ::
  forall (b :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
  b ->
  UTXO
withInlineDatum dat = mempty {utxoData = Just . InlineDatum . datafy $ dat}

{- | Specify reference script of a UTXO.

 @since 2.0.0
-}
withReferenceScript :: ScriptHash -> UTXO
withReferenceScript sh = mempty {utxoReferenceScript = Just sh}

{- | Associate a redeemer to a UTXO.

 Note that the redeemer is added to the final redeemer map only when it's in
  inputs and have both 'TxId' and 'TxIdx' explicitly provided by the user.

 @2.3.0
-}
withRedeemer ::
  forall (b :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
  b ->
  UTXO
withRedeemer r = mempty {utxoRedeemer = Just $ datafy r}

{- | Specify reference `TxId` of a UTXO.

 @since 2.0.0
-}
withRefTxId :: TxId -> UTXO
withRefTxId tid = mempty {utxoTxId = Just tid}

{- | Specify reference index of a UTXO.

 @since 2.0.0
-}
withRefIndex :: Integer -> UTXO
withRefIndex tidx = mempty {utxoTxIdx = Just tidx}

{- | Specify `TxOutRef` of a UTXO.

 @since 2.0.0
-}
withRef :: TxOutRef -> UTXO
withRef (TxOutRef tid idx) = withRefTxId tid <> withRefIndex idx

{- | Specify the `Value` of a UTXO. This will be monoidally merged `Value`s
 when given mutiple times.

 @since 2.0.0
-}
withValue :: Value -> UTXO
withValue val = mempty {utxoValue = val}

{- | Specify `StakingCredential` to a UTXO.

 @since 2.2.0
-}
withStakingCredential :: StakingCredential -> UTXO
withStakingCredential stak = mempty {utxoStakingCredential = Just stak}

{- | Specify `Address` of a UTXO.

 @since 2.2.0
-}
address :: Address -> UTXO
address Address {..} =
  mempty
    { utxoCredential = Just addressCredential
    , utxoStakingCredential = addressStakingCredential
    }

{- | Specify `Credential` of a UTXO.

 @since 2.0.0
-}
credential :: Credential -> UTXO
credential cred = mempty {utxoCredential = Just cred}

{- | Specify `PubKeyHash` of a UTXO.

 @since 2.0.0
-}
pubKey :: PubKeyHash -> UTXO
pubKey (PubKeyCredential -> cred) = mempty {utxoCredential = Just cred}

{- | Specify `ValidatorHash` of a UTXO.

 @since 2.0.0
-}
script :: ValidatorHash -> UTXO
script (ScriptCredential -> cred) = mempty {utxoCredential = Just cred}

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
  , bbMints :: Acc Mint
  , bbFee :: Value
  , bbTimeRange :: Interval POSIXTime
  , bbTxId :: TxId
  , bbRedeemers :: Acc (ScriptPurpose, Redeemer)
  }
  deriving stock (Show)

-- | @since 1.1.0
instance Semigroup BaseBuilder where
  BB ins refIns outs sigs dats ms fval range tid rs
    <> BB ins' refIns' outs' sigs' dats' ms' fval' range' tid' rs' =
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
        (rs <> rs')
      where
        choose f a b
          | f mempty == b = a
          | otherwise = b

-- | @since 1.1.0
instance Monoid BaseBuilder where
  mempty = BB mempty mempty mempty mempty mempty mempty mempty always (TxId "") mempty

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

 @since 2.0.0
-}
signedWith ::
  forall (a :: Type).
  (Builder a) =>
  PubKeyHash ->
  a
signedWith pkh = pack $ mempty {bbSignatures = pure pkh}

-- | @since 2.3.0
valueToMints ::
  forall r (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ r, PIsData p) =>
  r ->
  Value ->
  [Mint]
valueToMints r = fmap f . AssocMap.toList . getValue
  where
    f (cs, ts) = Mint cs (AssocMap.toList ts) $ datafy r

{- | Mint given value.

 @since 2.0.0
-}
mint ::
  forall (a :: Type).
  (Builder a) =>
  Value ->
  a
mint val = pack $ mempty {bbMints = fromReverseList $ valueToMints () val}

{- | Mint tokens with given redeemer.

 @since 2.3.0
-}
mintWith ::
  forall (a :: Type) (r :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ r, PIsData p, Builder a) =>
  r ->
  Value ->
  a
mintWith r val = pack $ mempty {bbMints = fromReverseList $ valueToMints r val}

{- Mint singleton valuw with given redeemer.

 @since 2.3.0
-}
mintSingletonWith ::
  forall (a :: Type) (r :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ r, PIsData p, Builder a) =>
  r ->
  CurrencySymbol ->
  TokenName ->
  Integer ->
  a
mintSingletonWith r cs tn q = mintWith r $ Value.singleton cs tn q

{- | Append extra datum to @ScriptContex@.

 @since 2.0.0
-}
extraData ::
  forall (a :: Type) (d :: Type) (p :: S -> Type).
  (Builder a, PUnsafeLiftDecl p, PLifted p ~ d, PIsData p) =>
  d ->
  a
extraData x = pack $ mempty {bbDatums = pure . datafy $ x}

extraRedeemer ::
  forall (a :: Type) (r :: Type) (p :: S -> Type).
  (Builder a, PUnsafeLiftDecl p, PLifted p ~ r, PIsData p) =>
  ScriptPurpose ->
  r ->
  a
extraRedeemer p r = pack $ mempty {bbRedeemers = pure (p, Redeemer $ BuiltinData $ datafy r)}

{- | Specify `TxId` of a script context.

 @since 2.0.0
-}
txId ::
  forall (a :: Type).
  (Builder a) =>
  TxId ->
  a
txId tid = pack $ mempty {bbTxId = tid}

{- | Specify transaction fee of a script context.

 @since 2.0.0
-}
fee ::
  forall (a :: Type).
  (Builder a) =>
  Value ->
  a
fee val = pack $ mempty {bbFee = val}

{- | Specify time range of a script context.

 @since 2.0.0
-}
timeRange ::
  forall (a :: Type).
  (Builder a) =>
  Interval POSIXTime ->
  a
timeRange r = pack $ mempty {bbTimeRange = r}

{- | Specify an output of a script context.

 @since 2.0.0
-}
output ::
  forall (a :: Type).
  (Builder a) =>
  UTXO ->
  a
output x = pack mempty {bbOutputs = pure x}

{- | Specify an input of a script context.

 @since 2.0.0
-}
input ::
  forall (a :: Type).
  (Builder a) =>
  UTXO ->
  a
input x = pack mempty {bbInputs = pure x}

{- | Specify a reference input of a script context.

 @since 2.0.0
-}
referenceInput ::
  forall (a :: Type).
  (Builder a) =>
  UTXO ->
  a
referenceInput x = pack mempty {bbReferenceInputs = pure x}

{- | Provide base @TxInfo@ to Continuation Monad.

 @since 1.1.0
-}
yieldBaseTxInfo ::
  (Builder b) => b -> TxInfo
yieldBaseTxInfo (unpack -> BB {..}) =
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
  Acc Mint ->
  Value
yieldMint = foldMap mintToValue . toList

{- | Convert a 'Mint' to a 'Value'.

     @since 2.3.0
-}
mintToValue :: Mint -> Value
mintToValue m =
  foldMap f (mintTokens m) <> Value.singleton adaSymbol adaToken 0
  where
    f = uncurry $ Value.singleton $ mintSymbol m

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
    mkTxInInfo utxo@UTXO {..} = TxInInfo ref (utxoToTxOut utxo)
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

{- | Provide script purpose - redeemer map for outputs to
 Continutation Monad.

     @since 2.3.0
-}
yieldRedeemerMap ::
  Acc UTXO ->
  Acc Mint ->
  [(ScriptPurpose, Redeemer)]
yieldRedeemerMap au am = scriptInputs <> mints
  where
    utxoToRedeemerPair :: UTXO -> Maybe (ScriptPurpose, Redeemer)
    utxoToRedeemerPair u = do
      refId <- utxoTxId u
      refIdx <- utxoTxIdx u
      let txRef = TxOutRef refId refIdx
      r <- utxoRedeemer u
      pure (Spending txRef, Redeemer $ BuiltinData r)

    scriptInputs :: [(ScriptPurpose, Redeemer)]
    scriptInputs = mapMaybe utxoToRedeemerPair $ toList au

    mintToRedeemerPair :: Mint -> (ScriptPurpose, Redeemer)
    mintToRedeemerPair m =
      (Minting $ mintSymbol m, Redeemer $ BuiltinData $ mintRedeemer m)

    mints :: [(ScriptPurpose, Redeemer)]
    mints = mintToRedeemerPair <$> toList am

{- | Automatically generate `TxOutRef`s from given builder.
 It tries to reserve index if given one; otherwise, it grants
 incremental indices to each inputs starting from one. If there
 are duplicate reserved indices, the second occurance will be
 treated as non-reserved and given incremental index.

 @since 2.1.0
-}
mkOutRefIndices ::
  forall (a :: Type).
  Builder a =>
  a ->
  a
mkOutRefIndices = over _bb go
  where
    go bb@BB {..} = bb {bbInputs = grantIndex bbInputs}

    grantIndex :: Acc UTXO -> Acc UTXO
    grantIndex (toList -> xs) = fromList $ grantIndex' 1 [] xs

    grantIndex' :: Integer -> [Integer] -> [UTXO] -> [UTXO]
    grantIndex' top used (x@(UTXO _ _ _ _ _ _ Nothing _) : xs)
      | top `elem` used = grantIndex' (top + 1) used (x : xs)
      | otherwise = x {utxoTxIdx = Just top} : grantIndex' (top + 1) (top : used) xs
    grantIndex' top used (x@(UTXO _ _ _ _ _ _ (Just i) _) : xs)
      | i `elem` used = grantIndex' top used (x {utxoTxIdx = Nothing} : xs)
      | otherwise = x : grantIndex' top (i : used) xs
    grantIndex' _ _ [] = []

{- | Given a list of pairs, combine second element of pair when first
     element is equal, using the given concat function. The output
     will be the reverse of what's given.

 @since 2.4.0
-}
combinePair ::
  forall (k :: Type) (v :: Type).
  Eq k =>
  (v -> v -> v) ->
  (k, v) ->
  [(k, v)] ->
  [(k, v)]
combinePair _ p [] = [p]
combinePair c (k, v) ((k', v') : xs)
  | k == k' = (k, c v v') : xs
  | otherwise = (k', v') : combinePair c (k, v) xs

{- | Given an 'AssocMap', combine all values when the key is equal,
     using the given concat function. The output will be the reverse
     of what's given.

 @since 2.4.0
-}
combineMap ::
  forall (k :: Type) (v :: Type).
  Eq k =>
  (v -> v -> v) ->
  Map k v ->
  Map k v
combineMap c (AssocMap.toList -> m) =
  AssocMap.fromList $ foldr (combinePair c) [] m

{- | Sort given 'AssocMap' by given comparator.

 @since 2.4.0
-}
sortMap ::
  forall (k :: Type) (v :: Type).
  Ord k =>
  Map k v ->
  Map k v
sortMap (AssocMap.toList -> m) =
  AssocMap.fromList $ sortBy (\(k, _) (k', _) -> compare k k') m

{- | Normalize and sort 'Value'.

    - 'Value' entries that match on both the 'CurrencySymbol' and
      'TokenName' are combined, with their amounts added together
    - 'Value' entries in the outer map are sorted by currency symbol
    - 'Value' entries in the inner map are sorted by token name
    - Non-ADA zero-entries are dropped
    - If the `Value` is missing an ADA entry, as 0 ADA entries is added

 @since 2.4.0
-}
normalizeValue :: Value -> Value
normalizeValue (getValue -> val) =
  let val' =
        if isNothing $ AssocMap.lookup "" val
          then AssocMap.insert adaSymbol (AssocMap.fromList [(adaToken, 0)]) val
          else val
   in Value.Value
        . AssocMap.filter (/= AssocMap.empty)
        . sortMap
        . AssocMap.mapWithKey
          ( \cs tm ->
              sortMap $
                AssocMap.mapMaybeWithKey
                  ( \tk v ->
                      if v /= 0 || (tk == adaToken && cs == adaSymbol)
                        then Just v
                        else Nothing
                  )
                  tm
          )
        . combineMap (AssocMap.unionWith (+))
        $ val'

{- | Normalize all values and mints in the given builder.

 @since 2.4.0
-}
mkNormalizedBase ::
  forall (a :: Type).
  Builder a =>
  a ->
  a
mkNormalizedBase = over _bb go
  where
    go bb@BB {..} =
      bb
        { bbInputs = normalizeUTXO <$> bbInputs
        , bbReferenceInputs = normalizeUTXO <$> bbReferenceInputs
        , bbOutputs = normalizeUTXO <$> bbOutputs
        , bbMints = normalizeMint <$> bbMints
        }
