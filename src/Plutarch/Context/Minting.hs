{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Context.Minting (
    -- * Types
    MintingBuilder,
    Tokens (Tokens),
    MintingAction (..),

    -- * Functions

    -- ** Construction
    mkTokens,

    -- ** Describe inputs
    inputFromPubKey,
    inputFromPubKeyWith,
    inputTokensFromPubKey,
    inputTokensFromPubKeyWith,
    inputFromOtherScript,
    inputTokensFromOtherScript,

    -- ** Describe outputs
    outputToPubKey,
    outputToPubKeyWith,
    outputTokensToPubKey,
    outputTokensToPubKeyWith,
    outputToOtherScript,
    outputTokensToOtherScript,

    -- ** Describe others
    mintForeign,
    signedWith,
    extraData,

    -- ** Compilation
    mintingContext,
) where

import Acc (Acc)
import qualified Acc
import Control.Monad (foldM, guard)
import Data.Foldable (toList, traverse_)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Plutarch (S)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Context.Internal (
    Minting (Minting),
    MintingBuilder (
        MB,
        sbDatums,
        sbInputs,
        sbMints,
        sbOutputs,
        sbSignatures
    ),
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
    ScriptContext (ScriptContext),
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
import qualified Plutus.V1.Ledger.Contexts as Contexts
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (
    Datum (Datum),
    DatumHash,
    ValidatorHash,
 )
import Plutus.V1.Ledger.Value (
    CurrencySymbol,
    TokenName,
    Value,
    symbols,
 )
import qualified Plutus.V1.Ledger.Value as Value
import PlutusCore.Data (Data)

{- | Some positive number of tokens, with the given name, controlled by the
 minting policy for which the script context is being built.

 @since 1.0.0
-}
data Tokens = T TokenName Integer
    deriving stock
        ( -- | @since 1.0.0
          Eq
        , -- | @since 1.0.0
          Show
        )

{- | Attempt to make a 'Tokens' with the given name and amount. Fails if given a
 non-positive amount.

 @since 1.0.0
-}
mkTokens :: TokenName -> Integer -> Maybe Tokens
mkTokens name amount = T name <$> (guard (amount > 0) $> amount)

{- | You can use this pattern as a constructor, but /only/ for matching.

 @since 1.0.0
-}
pattern Tokens :: TokenName -> Integer -> Tokens
pattern Tokens name amount <- T name amount

{-# COMPLETE Tokens #-}

{- | Describes what we want a minting policy to do.

 @since 1.0.0
-}
data MintingAction = Mint Tokens | Burn Tokens
    deriving stock
        ( -- | @since 1.0.0
          Eq
        , -- | @since 1.0.0
          Show
        )

{- | Describes a single input from a 'PubKey'.

 @since 1.0.0
-}
inputFromPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Value ->
    MintingBuilder redeemer
inputFromPubKey pkh val = input go
  where
    go :: SideUTXO
    go = SideUTXO (PubKeyUTXO pkh Nothing) (GeneralValue val)

{- | Describes a single input from a 'PubKey', where some additional data is
 also provided.

 @since 1.0.0
-}
inputFromPubKeyWith ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Value ->
    a ->
    MintingBuilder redeemer
inputFromPubKeyWith pkh val x = input go
  where
    go :: SideUTXO
    go = SideUTXO (PubKeyUTXO pkh . Just . datafy $ x) (GeneralValue val)

{- | Describes a single input of 'Tokens' from a 'PubKey'.

 @since 1.0.0
-}
inputTokensFromPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Tokens ->
    MintingBuilder redeemer
inputTokensFromPubKey pkh (T name amount) = input go
  where
    go :: SideUTXO
    go = SideUTXO (PubKeyUTXO pkh Nothing) (TokensValue name amount)

{- | As 'inputTokensFromPubKey', but with some extra data as well.

 @since 1.0.0
-}
inputTokensFromPubKeyWith ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Tokens ->
    a ->
    MintingBuilder redeemer
inputTokensFromPubKeyWith pkh (T name amount) x = input go
  where
    go :: SideUTXO
    go =
        let val = TokensValue name amount
            typ = PubKeyUTXO pkh . Just . datafy $ x
         in SideUTXO typ val

{- | Describes an input from a script /other/ than the one for which the context
 is being made.

 @since 1.0.0
-}
inputFromOtherScript ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    Value ->
    a ->
    MintingBuilder redeemer
inputFromOtherScript vh val x = input go
  where
    go :: SideUTXO
    go = SideUTXO (ScriptUTXO vh . datafy $ x) (GeneralValue val)

{- | As 'inputFromOtherScript', except the input is a 'Tokens'.

 @since 1.0.0
-}
inputTokensFromOtherScript ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    Tokens ->
    a ->
    MintingBuilder redeemer
inputTokensFromOtherScript vh (T name amount) x = input go
  where
    go :: SideUTXO
    go = SideUTXO (ScriptUTXO vh . datafy $ x) (TokensValue name amount)

{- | Describes a single output to a 'PubKey'.

 @since 1.0.0
-}
outputToPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Value ->
    MintingBuilder redeemer
outputToPubKey pkh val = output go
  where
    go :: SideUTXO
    go = SideUTXO (PubKeyUTXO pkh Nothing) (GeneralValue val)

{- | Describes a single output of 'Tokens' to a 'PubKey'.

 @since 1.0.0
-}
outputTokensToPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Tokens ->
    MintingBuilder redeemer
outputTokensToPubKey pkh (T name amount) = output go
  where
    go :: SideUTXO
    go =
        let val = TokensValue name amount
         in SideUTXO (PubKeyUTXO pkh Nothing) val

{- | As 'outputToPubKey', but adds some extra data as well.

 @since 1.0.0
-}
outputToPubKeyWith ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Value ->
    a ->
    MintingBuilder redeemer
outputToPubKeyWith phk val x = output go
  where
    go :: SideUTXO
    go = SideUTXO (PubKeyUTXO phk . Just . datafy $ x) (GeneralValue val)

{- | As 'outputTokensToPubKey', but adds some extra data as well.

 @since 1.0.0
-}
outputTokensToPubKeyWith ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Tokens ->
    a ->
    MintingBuilder redeemer
outputTokensToPubKeyWith pkh (T name amount) x = output go
  where
    go :: SideUTXO
    go =
        let val = TokensValue name amount
            typ = PubKeyUTXO pkh . Just . datafy $ x
         in SideUTXO typ val

{- | Describe an output to a script /other/ than the one for which this script
 context is being made.

 @since 1.0.0
-}
outputToOtherScript ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    Value ->
    a ->
    MintingBuilder redeemer
outputToOtherScript vh val x = output go
  where
    go :: SideUTXO
    go =
        let value = GeneralValue val
            typ = ScriptUTXO vh . datafy $ x
         in SideUTXO typ value

{- | As 'outputToOtherScript' except that the output is a 'Tokens'.

 @since 1.0.0
-}
outputTokensToOtherScript ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    Tokens ->
    a ->
    MintingBuilder redeemer
outputTokensToOtherScript vh (T name amount) x = output go
  where
    go :: SideUTXO
    go =
        let value = TokensValue name amount
            typ = ScriptUTXO vh . datafy $ x
         in SideUTXO typ value

{- | Describe that we've signed with one signature.

 @since 1.0.0
-}
signedWith ::
    forall (redeemer :: Type).
    PubKeyHash ->
    MintingBuilder redeemer
signedWith pkh = MB mempty mempty (pure pkh) mempty mempty

{- | Describe a mint of tokens /not/ controlled by the minting policy for which
 this script context is being made.

 @since 1.0.0
-}
mintForeign ::
    forall (redeemer :: Type).
    Value ->
    MintingBuilder redeemer
mintForeign = MB mempty mempty mempty mempty . pure . Minting

{- | Describe that there is an additional piece of data in the context.

 @since 1.0.0
-}
extraData ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    MintingBuilder redeemer
extraData x = MB mempty mempty mempty (pure . datafy $ x) mempty

{- | Construct a 'ScriptContext' for minting, given a configuration and a list
 of things to do.

 @since 1.0.0
-}
mintingContext ::
    forall (redeemer :: Type).
    TransactionConfig ->
    MintingBuilder redeemer ->
    NonEmpty MintingAction ->
    Maybe ScriptContext
mintingContext conf builder acts =
    ScriptContext <$> go <*> (pure . Contexts.Minting $ sym)
  where
    sym :: CurrencySymbol
    sym = testCurrencySymbol conf
    go :: Maybe TxInfo
    go = case baseTxInfo conf builder of
        Nothing -> Nothing
        Just baseInfo ->
            Just $ baseInfo{txInfoMint = mintingValue <> txInfoMint baseInfo}
    mintingValue :: Value
    mintingValue = foldMap (processAction sym) acts

-- Helpers

processAction :: CurrencySymbol -> MintingAction -> Value
processAction sym =
    uncurry (Value.singleton sym) . \case
        Mint (Tokens name amount) -> (name, amount)
        Burn (Tokens name amount) -> (name, negate amount)

baseTxInfo ::
    forall (redeemer :: Type).
    TransactionConfig ->
    MintingBuilder redeemer ->
    Maybe TxInfo
baseTxInfo conf builder = do
    value <- foldM toUsefulValue mempty . sbMints $ builder
    insList <- toList <$> (foldM checkSideAndCombine mempty . sbInputs $ builder)
    outsList <- toList <$> (foldM checkSideAndCombine mempty . sbOutputs $ builder)
    let signatures = toList . sbSignatures $ builder
    let dats = toList . fmap datumWithHash . sbDatums $ builder
    pure $
        starterTxInfo
            { txInfoMint = value
            , txInfoInputs = createTxInInfos insList
            , txInfoOutputs = createTxOuts outsList
            , txInfoSignatories = signatures
            , txInfoData =
                mapMaybe sideUtxoToDatum insList
                    <> mapMaybe sideUtxoToDatum outsList
                    <> dats
            }
  where
    toUsefulValue :: Value -> Minting -> Maybe Value
    toUsefulValue acc (Minting val) =
        (acc <>) <$> ((traverse_ (\sym -> guard (sym /= ourSym)) . symbols $ val) $> val)
    ourSym :: CurrencySymbol
    ourSym = testCurrencySymbol conf
    ourAddress :: Address
    ourAddress = scriptHashAddress . testValidatorHash $ conf
    checkSideAndCombine :: Acc SideUTXO -> SideUTXO -> Maybe (Acc SideUTXO)
    checkSideAndCombine acc side@(SideUTXO typ _) = do
        let sideAddress = case typ of
                PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
                ScriptUTXO hash _ -> scriptHashAddress hash
        Acc.cons <$> (guard (sideAddress /= ourAddress) $> side) <*> pure acc
    starterTxInfo :: TxInfo
    starterTxInfo =
        TxInfo
            { txInfoInputs = mempty
            , txInfoOutputs = mempty
            , txInfoFee = testFee conf
            , txInfoMint = mempty
            , txInfoDCert = mempty
            , txInfoWdrl = mempty
            , txInfoValidRange = testTimeRange conf
            , txInfoSignatories = mempty
            , txInfoData = mempty
            , txInfoId = TxId "testTx"
            }
    createTxInInfos :: [SideUTXO] -> [TxInInfo]
    createTxInInfos xs =
        let outRefs = TxOutRef (testTxId conf) <$> [1 ..]
         in zipWith TxInInfo outRefs . fmap (sideToTxOut ourSym) $ xs
    createTxOuts :: [SideUTXO] -> [TxOut]
    createTxOuts = fmap (sideToTxOut ourSym)

sideUtxoToDatum :: SideUTXO -> Maybe (DatumHash, Datum)
sideUtxoToDatum (SideUTXO typ _) = case typ of
    ScriptUTXO _ dat -> Just . datumWithHash $ dat
    PubKeyUTXO _ dat -> datumWithHash <$> dat

datumWithHash :: Data -> (DatumHash, Datum)
datumWithHash d = (datumHash dt, dt)
  where
    dt :: Datum
    dt = Datum . BuiltinData $ d

extractValue :: CurrencySymbol -> ValueType -> Value
extractValue sym = \case
    GeneralValue val -> val
    TokensValue name amount -> Value.singleton sym name amount

sideToTxOut :: CurrencySymbol -> SideUTXO -> TxOut
sideToTxOut sym (SideUTXO typ val) =
    let value = extractValue sym val
     in case typ of
            PubKeyUTXO pkh mDat ->
                TxOut (pubKeyHashAddress pkh) value $ datumHash . dataToDatum <$> mDat
            ScriptUTXO hash dat ->
                TxOut (scriptHashAddress hash) value . Just . datumHash . dataToDatum $ dat

dataToDatum :: Data -> Datum
dataToDatum = Datum . BuiltinData

input ::
    forall (redeemer :: Type).
    SideUTXO ->
    MintingBuilder redeemer
input x = MB (pure x) mempty mempty mempty mempty

output ::
    forall (redeemer :: Type).
    SideUTXO ->
    MintingBuilder redeemer
output x = MB mempty (pure x) mempty mempty mempty

datafy ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    Data
datafy x = plift (pforgetData (pdata (pconstant x)))
