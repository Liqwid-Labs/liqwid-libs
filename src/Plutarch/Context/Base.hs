{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
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
  BaseBuilder,
  UTXO,
  Mint,

  -- * Modular constructors
  output,
  input,
  continuing,
  continuingWith,
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
  mkMint,

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
import Data.List (sortBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Last (Last))
import GHC.Exts (coerce, fromList)
import Optics (
  A_Lens,
  LabelOptic (labelOptic),
  Lens',
  lens,
  over,
  set,
  view,
 )
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

     @since 2.5.0
-}
data Mint = Mint CurrencySymbol [(TokenName, Integer)] Data
  deriving stock
    ( -- | @since 2.3.0
      Show
    , -- | @since 2.4.0
      Eq
    )

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ CurrencySymbol, b ~ CurrencySymbol) =>
  LabelOptic "symbol" k Mint Mint a b
  where
  labelOptic = lens (\(Mint cs _ _) -> cs) $ \(Mint _ toks red) cs' ->
    Mint cs' toks red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ [(TokenName, Integer)], b ~ [(TokenName, Integer)]) =>
  LabelOptic "tokens" k Mint Mint a b
  where
  labelOptic = lens (\(Mint _ toks _) -> toks) $ \(Mint cs _ red) toks' ->
    Mint cs toks' red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Data, b ~ Data) =>
  LabelOptic "redeemer" k Mint Mint a b
  where
  labelOptic = lens (\(Mint _ _ red) -> red) $ \(Mint cs toks _) red' ->
    Mint cs toks red'

-- | @since 2.5.0
mkMint :: CurrencySymbol -> [(TokenName, Integer)] -> Data -> Mint
mkMint = Mint

{- | Normalize mint amount.

 @since 2.4.0
-}
normalizeMint :: Mint -> Mint
normalizeMint =
  over #tokens (sortOn fst . filter ((/= 0) . snd) . foldr (combinePair (+)) [])

{- | 'UTXO' is used to represent any input or output of a transaction in the builders.

A UTXO' contains:

  - A `Value` of one or more assets.
  - An 'Address' that it exists at.
  - Potentially some Plutus 'Data'.

This is different from 'TxOut', in that we store the 'Data' fully instead of the 'DatumHash'.

 @since 2.5.0
-}
data UTXO
  = UTXO
      (Last Credential)
      (Last StakingCredential)
      Value
      (Last DatumType)
      (Last ScriptHash)
      (Last TxId)
      (Last Integer)
      (Last Data)
  deriving stock
    ( -- | @since 2.0.0
      Show
    )

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe Credential, b ~ Maybe Credential) =>
  LabelOptic "credential" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO x _ _ _ _ _ _ _) -> coerce x) $ \(UTXO _ scred val dat rs ti tix red) cred' ->
    UTXO (coerce cred') scred val dat rs ti tix red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe StakingCredential, b ~ Maybe StakingCredential) =>
  LabelOptic "stakingCredential" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ x _ _ _ _ _ _) -> coerce x) $ \(UTXO cred _ val dat rs ti tix red) scred' ->
    UTXO cred (coerce scred') val dat rs ti tix red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Value, b ~ Value) =>
  LabelOptic "value" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ _ x _ _ _ _ _) -> x) $ \(UTXO cred scred _ dat rs ti tix red) val' ->
    UTXO cred scred val' dat rs ti tix red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe DatumType, b ~ Maybe DatumType) =>
  LabelOptic "data" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ _ _ x _ _ _ _) -> coerce x) $ \(UTXO cred scred val _ rs ti tix red) dat' ->
    UTXO cred scred val (coerce dat') rs ti tix red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe ScriptHash, b ~ Maybe ScriptHash) =>
  LabelOptic "referenceScript" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ _ _ _ x _ _ _) -> coerce x) $ \(UTXO cred scred val dat _ ti tix red) rs' ->
    UTXO cred scred val dat (coerce rs') ti tix red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe TxId, b ~ Maybe TxId) =>
  LabelOptic "txId" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ _ _ _ _ x _ _) -> coerce x) $ \(UTXO cred scred val dat rs _ tix red) ti' ->
    UTXO cred scred val dat rs (coerce ti') tix red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe Integer, b ~ Maybe Integer) =>
  LabelOptic "txIdx" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ _ _ _ _ _ x _) -> coerce x) $ \(UTXO cred scred val dat rs ti _ red) tix' ->
    UTXO cred scred val dat rs ti (coerce tix') red

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Maybe Data, b ~ Maybe Data) =>
  LabelOptic "redeemer" k UTXO UTXO a b
  where
  labelOptic = lens (\(UTXO _ _ _ _ _ _ _ x) -> coerce x) $ \(UTXO cred scred val dat rs ti tix _) red' ->
    UTXO cred scred val dat rs ti tix . coerce $ red'

-- | @since 2.0.0
instance Semigroup UTXO where
  (UTXO c stc v d rs t tidx r) <> (UTXO c' stc' v' d' rs' t' tidx' r') =
    UTXO
      (c <> c')
      (stc <> stc')
      (v <> v')
      (d <> d')
      (rs <> rs')
      (t <> t')
      (tidx <> tidx')
      (r <> r')

-- | @since 2.0.0
instance Monoid UTXO where
  mempty = UTXO mempty mempty mempty mempty mempty mempty mempty mempty

{- | Pulls address output of given UTXO'

 @since 1.1.0
-}
utxoAddress :: UTXO -> Address
utxoAddress utxo =
  Address (fromMaybe (PubKeyCredential . PubKeyHash $ "") . view #credential $ utxo)
    . view #stakingCredential
    $ utxo

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d

utxoOutputDatum :: UTXO -> OutputDatum
utxoOutputDatum utxo = case view #data utxo of
  Nothing -> NoOutputDatum
  Just (InlineDatum d) -> OutputDatum . Datum . BuiltinData $ d
  Just (ContextDatum d) -> OutputDatumHash . datumHash . Datum . BuiltinData $ d

utxoDatumPair :: UTXO -> Maybe (DatumHash, Datum)
utxoDatumPair utxo = do
  dat <- view #data utxo
  case dat of
    InlineDatum _ -> Nothing
    ContextDatum d -> Just . datumWithHash $ d

{- | Normalize the value 'UTXO' holds.

 @since 2.4.0
-}
normalizeUTXO :: UTXO -> UTXO
normalizeUTXO = over #value normalizeValue

{- | Construct TxOut of given UTXO

 @since 1.1.0
-}
utxoToTxOut ::
  UTXO ->
  TxOut
utxoToTxOut utxo =
  TxOut (utxoAddress utxo) (view #value utxo) (utxoOutputDatum utxo)
    . view #referenceScript
    $ utxo

{- | Specify datum of a UTXO.

 @since 2.0.0
-}
withDatum ::
  forall (b :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
  b ->
  UTXO
withDatum dat =
  set #data (pure . ContextDatum . datafy $ dat) (mempty :: UTXO)

{- | Specify in-line datum of a UTXO.

 @since 2.0.0
-}
withInlineDatum ::
  forall (b :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
  b ->
  UTXO
withInlineDatum dat =
  set #data (pure . InlineDatum . datafy $ dat) (mempty :: UTXO)

{- | Specify reference script of a UTXO.

 @since 2.0.0
-}
withReferenceScript :: ScriptHash -> UTXO
withReferenceScript sh =
  set #referenceScript (pure sh) (mempty :: UTXO)

{- | Associate a redeemer to a UTXO.

 Note that the redeemer is added to the final redeemer map only when it's in
  inputs and have both 'TxId' and 'TxIdx' explicitly provided by the user.

 @since 2.3.0
-}
withRedeemer ::
  forall (b :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ b, PIsData p) =>
  b ->
  UTXO
withRedeemer r =
  set #redeemer (pure . datafy $ r) (mempty :: UTXO)

{- | Specify reference `TxId` of a UTXO.

 @since 2.0.0
-}
withRefTxId :: TxId -> UTXO
withRefTxId tid =
  set #txId (pure tid) (mempty :: UTXO)

{- | Specify reference index of a UTXO.

 @since 2.0.0
-}
withRefIndex :: Integer -> UTXO
withRefIndex tidx =
  set #txIdx (pure tidx) (mempty :: UTXO)

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
withValue val = set #value val (mempty :: UTXO)

{- | Specify `StakingCredential` to a UTXO.

 @since 2.2.0
-}
withStakingCredential :: StakingCredential -> UTXO
withStakingCredential stak =
  set #stakingCredential (pure stak) (mempty :: UTXO)

{- | Specify `Address` of a UTXO.

 @since 2.2.0
-}
address :: Address -> UTXO
address Address {..} =
  set #credential (Just addressCredential)
    . set #stakingCredential addressStakingCredential
    $ (mempty :: UTXO)

{- | Specify `Credential` of a UTXO.

 @since 2.0.0
-}
credential :: Credential -> UTXO
credential cred = set #credential (pure cred) (mempty :: UTXO)

{- | Specify `PubKeyHash` of a UTXO.

 @since 2.0.0
-}
pubKey :: PubKeyHash -> UTXO
pubKey (PubKeyCredential -> cred) =
  set #credential (pure cred) (mempty :: UTXO)

{- | Specify `ValidatorHash` of a UTXO.

 @since 2.0.0
-}
script :: ValidatorHash -> UTXO
script (ScriptCredential -> cred) =
  set #credential (pure cred) (mempty :: UTXO)

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
 'ScriptContext' creation, leaving specific builders only with
 minimal logical checkings.

 @since 2.5.0
-}
data BaseBuilder
  = BB
      (Acc UTXO)
      (Acc UTXO)
      (Acc UTXO)
      (Acc PubKeyHash)
      (Acc Data)
      (Acc Mint)
      Value
      (Interval POSIXTime)
      TxId
      (Acc (ScriptPurpose, Redeemer))
  deriving stock (Show)

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc UTXO, b ~ Acc UTXO) =>
  LabelOptic "inputs" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB x _ _ _ _ _ _ _ _ _) -> x) $ \(BB _ rins outs sigs dats ms f tr txi reds) ins' ->
    BB ins' rins outs sigs dats ms f tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc UTXO, b ~ Acc UTXO) =>
  LabelOptic "referenceInputs" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ x _ _ _ _ _ _ _ _) -> x) $ \(BB ins _ outs sigs dats ms f tr txi reds) rins' ->
    BB ins rins' outs sigs dats ms f tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc UTXO, b ~ Acc UTXO) =>
  LabelOptic "outputs" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ x _ _ _ _ _ _ _) -> x) $ \(BB ins rins _ sigs dats ms f tr txi reds) outs' ->
    BB ins rins outs' sigs dats ms f tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc PubKeyHash, b ~ Acc PubKeyHash) =>
  LabelOptic "signatures" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ x _ _ _ _ _ _) -> x) $ \(BB ins rins outs _ dats ms f tr txi reds) sigs' ->
    BB ins rins outs sigs' dats ms f tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc Data, b ~ Acc Data) =>
  LabelOptic "datums" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ _ x _ _ _ _ _) -> x) $ \(BB ins rins outs sigs _ ms f tr txi reds) dats' ->
    BB ins rins outs sigs dats' ms f tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc Mint, b ~ Acc Mint) =>
  LabelOptic "mints" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ _ _ x _ _ _ _) -> x) $ \(BB ins rins outs sigs dats _ f tr txi reds) ms' ->
    BB ins rins outs sigs dats ms' f tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Value, b ~ Value) =>
  LabelOptic "fee" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ _ _ _ x _ _ _) -> x) $ \(BB ins rins outs sigs dats ms _ tr txi reds) f' ->
    BB ins rins outs sigs dats ms f' tr txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Interval POSIXTime, b ~ Interval POSIXTime) =>
  LabelOptic "timeRange" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ _ _ _ _ x _ _) -> x) $ \(BB ins rins outs sigs dats ms f _ txi reds) tr' ->
    BB ins rins outs sigs dats ms f tr' txi reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ TxId, b ~ TxId) =>
  LabelOptic "txId" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ _ _ _ _ _ x _) -> x) $ \(BB ins rins outs sigs dats ms f tr _ reds) txi' ->
    BB ins rins outs sigs dats ms f tr txi' reds

-- | @since 2.5.0
instance
  (k ~ A_Lens, a ~ Acc (ScriptPurpose, Redeemer), b ~ Acc (ScriptPurpose, Redeemer)) =>
  LabelOptic "redeemers" k BaseBuilder BaseBuilder a b
  where
  labelOptic = lens (\(BB _ _ _ _ _ _ _ _ _ x) -> x) $ \(BB ins rins outs sigs dats ms f tr txi _) reds' ->
    BB ins rins outs sigs dats ms f tr txi reds'

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
        (choose #timeRange range range')
        (choose #txId tid tid')
        (rs <> rs')
      where
        choose ::
          forall (a :: Type).
          (Eq a) =>
          Lens' BaseBuilder a ->
          a ->
          a ->
          a
        choose ell x y
          | view ell mempty == y = x
          | otherwise = y

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
signedWith pkh = pack . set #signatures (pure pkh) $ (mempty :: BaseBuilder)

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
mint = mintWith ()

{- | Mint tokens with given redeemer.

 @since 2.3.0
-}
mintWith ::
  forall (a :: Type) (r :: Type) (p :: S -> Type).
  (PUnsafeLiftDecl p, PLifted p ~ r, PIsData p, Builder a) =>
  r ->
  Value ->
  a
mintWith r val =
  pack . set #mints (fromReverseList . valueToMints r $ val) $
    (mempty :: BaseBuilder)

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
extraData x =
  pack . set #datums (pure . datafy $ x) $ (mempty :: BaseBuilder)

extraRedeemer ::
  forall (a :: Type) (r :: Type) (p :: S -> Type).
  (Builder a, PUnsafeLiftDecl p, PLifted p ~ r, PIsData p) =>
  ScriptPurpose ->
  r ->
  a
extraRedeemer p r =
  pack
    . set #redeemers (pure (p, Redeemer . BuiltinData . datafy $ r))
    $ (mempty :: BaseBuilder)

{- | Specify `TxId` of a script context.

 @since 2.0.0
-}
txId ::
  forall (a :: Type).
  (Builder a) =>
  TxId ->
  a
txId tid = pack . set #txId tid $ (mempty :: BaseBuilder)

{- | Specify transaction fee of a script context.

 @since 2.0.0
-}
fee ::
  forall (a :: Type).
  (Builder a) =>
  Value ->
  a
fee val = pack . set #fee val $ (mempty :: BaseBuilder)

{- | Specify time range of a script context.

 @since 2.0.0
-}
timeRange ::
  forall (a :: Type).
  (Builder a) =>
  Interval POSIXTime ->
  a
timeRange r = pack . set #timeRange r $ (mempty :: BaseBuilder)

{- | Specify an output of a script context.

 @since 2.0.0
-}
output ::
  forall (a :: Type).
  (Builder a) =>
  UTXO ->
  a
output x = pack . set #outputs (pure x) $ (mempty :: BaseBuilder)

{- | Specify an input of a script context.

 @since 2.0.0
-}
input ::
  forall (a :: Type).
  (Builder a) =>
  UTXO ->
  a
input x = pack . set #inputs (pure x) $ (mempty :: BaseBuilder)

{- | Specify a reference input of a script context.

 @since 2.0.0
-}
referenceInput ::
  forall (a :: Type).
  (Builder a) =>
  UTXO ->
  a
referenceInput x =
  pack . set #referenceInputs (pure x) $ (mempty :: BaseBuilder)

{- | As 'continuingWith', but assumes the \'continued\' 'Value' does not change.
 Useful for state tokens.

 @since 2.6.1
-}
continuing ::
  forall (a :: Type).
  (Builder a, Semigroup a) =>
  -- | How the continued 'UTXO' should transform from input to output
  (UTXO -> UTXO) ->
  -- | Input 'UTXO'
  UTXO ->
  -- | 'Value' to continue through
  Value ->
  a
continuing = continuingWith id

{- | Specify that a 'UTXO' 'Value' should \'continue\'. More precisely, this
 acts as a combination of 'input' and 'output', where a \'Value\' is supposed
 to \'continue through\', possibly with modifications along the way. This also
 assumes that the input itself continues through, again, possibly with
 modifications.

 This function is designed to be maximally general. If you don't need the
 continued 'Value' to change, use 'continuing'; if you don't need the rest
 of the UTXO to change in the output, pass 'id'.

 = Note

 The 'Value' transformation function will affect only the continued 'Value';
 thus, if the UTXO was built with additional 'withValue's, those will not be
 affected by the application of the 'Value' transformation function between
 input and output.

 @since 2.6.1
-}
continuingWith ::
  forall (a :: Type).
  (Builder a, Semigroup a) =>
  -- | How the continued 'Value' should transform from input to output
  (Value -> Value) ->
  -- | How the continued 'UTXO' should transform from input to output
  (UTXO -> UTXO) ->
  -- | Input 'UTXO'
  UTXO ->
  -- | 'Value' to continue through
  Value ->
  a
continuingWith deltaV deltaU inUTXO val =
  input (withValue val <> inUTXO)
    <> output (withValue (deltaV val) <> deltaU inUTXO)

{- | Provide base @TxInfo@ to Continuation Monad.

 @since 1.1.0
-}
yieldBaseTxInfo ::
  forall (b :: Type).
  (Builder b) =>
  b ->
  TxInfo
yieldBaseTxInfo b = case unpack b of
  bb ->
    TxInfo
      { txInfoInputs = mempty
      , txInfoReferenceInputs = mempty
      , txInfoOutputs = mempty
      , txInfoFee = view #fee bb
      , txInfoMint = mempty
      , txInfoDCert = mempty
      , txInfoWdrl = AssocMap.fromList []
      , txInfoValidRange = view #timeRange bb
      , txInfoSignatories = mempty
      , txInfoRedeemers = AssocMap.fromList []
      , txInfoData = AssocMap.fromList []
      , txInfoId = view #txId bb
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
  foldMap f (view #tokens m) <> Value.singleton adaSymbol adaToken 0
  where
    f :: (TokenName, Integer) -> Value
    f = uncurry $ Value.singleton $ view #symbol m

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
    mkTxInInfo utxo =
      TxInInfo
        ( TxOutRef
            (fromMaybe "" . view #txId $ utxo)
            (fromMaybe 0 . view #txIdx $ utxo)
        )
        . utxoToTxOut
        $ utxo
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
      refId <- view #txId u
      refIdx <- view #txIdx u
      let txRef = TxOutRef refId refIdx
      r <- view #redeemer u
      pure (Spending txRef, Redeemer $ BuiltinData r)
    scriptInputs :: [(ScriptPurpose, Redeemer)]
    scriptInputs = mapMaybe utxoToRedeemerPair $ toList au
    mintToRedeemerPair :: Mint -> (ScriptPurpose, Redeemer)
    mintToRedeemerPair m =
      ( Minting $ view #symbol m
      , Redeemer $ BuiltinData $ view #redeemer m
      )
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
    go :: BaseBuilder -> BaseBuilder
    go = over #inputs grantIndex
    grantIndex :: Acc UTXO -> Acc UTXO
    grantIndex (toList -> xs) = fromList $ grantIndex' 1 [] xs
    grantIndex' :: Integer -> [Integer] -> [UTXO] -> [UTXO]
    grantIndex' top used (x@(UTXO _ _ _ _ _ _ (Last Nothing) _) : xs)
      | top `elem` used = grantIndex' (top + 1) used (x : xs)
      | otherwise = set #txIdx (pure top) x : grantIndex' (top + 1) (top : used) xs
    grantIndex' top used (x@(UTXO _ _ _ _ _ _ (Last (Just i)) _) : xs)
      | i `elem` used = grantIndex' top used (set #txIdx Nothing x : xs)
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

    NOTE: this function will silently drop values in the underlying
    map. Read below for more info.

    - 'Value' entries that match on both the 'CurrencySymbol' and
      'TokenName' are combined, with their amounts added together
    - 'Value' entries in the outer map are sorted by currency symbol
    - 'Value' entries in the inner map are sorted by token name
    - Non-ADA zero-entries are dropped
    - If the `Value` is missing an ADA entry, as 0 ADA entries is added
    - Non-Ada "TokenName"s under the Ada currency symbol are silently
      stripped.

 @since 2.4.0
-}
normalizeValue :: Value -> Value
normalizeValue (getValue -> val) =
  let valWith0Ada =
        AssocMap.insert
          adaSymbol
          (AssocMap.fromList [(adaToken, 0)])
          val
      val' = case AssocMap.lookup adaSymbol val of
        Nothing -> valWith0Ada -- No Ada symbol present, add the 0 entry
        Just adaTokenEntries -> case AssocMap.lookup adaToken adaTokenEntries of
          Nothing -> valWith0Ada -- No Ada token present, add the 0 entry
          Just adaAmount ->
            {- Ada symbol and token present, strip out ada symbol
               entires (which may contain non-Ada tokens and/or duplicates),
               and add back in only the correct Ada asset class amount
            -}
            AssocMap.insert
              adaSymbol
              (AssocMap.fromList [(adaToken, adaAmount)])
              ( AssocMap.fromList $
                  filter (\e -> fst e /= adaSymbol) (AssocMap.toList val)
              )
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
    go :: BaseBuilder -> BaseBuilder
    go =
      over #inputs (fmap normalizeUTXO)
        . over #referenceInputs (fmap normalizeUTXO)
        . over #outputs (fmap normalizeUTXO)
        . over #mints (fmap normalizeMint)
