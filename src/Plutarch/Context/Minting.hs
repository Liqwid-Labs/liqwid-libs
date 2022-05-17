{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- | Module: Plutarch.Context.Minting
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for minting contexts. 'MintingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'MintingBuilder' using '<>'.
-}
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
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Plutarch (S)
import Plutarch.Api.V1 (datumHash)
import Plutarch.Builtin (PIsData)
import Plutarch.Context.Base (
    BaseBuilder,
    SideUTXO (SideUTXO),
    UTXOType (PubKeyUTXO, ScriptUTXO),
 )
import qualified Plutarch.Context.Base as Base
import Plutarch.Context.Config (
    ContextConfig (
        configCurrencySymbol,
        configTxId,
        configValidatorHash
    ),
 )
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Address (Address, pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Api (BuiltinData (BuiltinData))
import Plutus.V1.Ledger.Contexts (
    ScriptContext (ScriptContext),
    ScriptPurpose (Minting),
    TxInInfo (TxInInfo),
    TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoOutputs),
    TxOut (TxOut),
    TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash)
import Plutus.V1.Ledger.Value (
    CurrencySymbol,
    TokenName,
    Value,
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

{- | A context builder for minting. Corresponds to minting policies (broadly),
 and the 'Plutus.V1.Ledger.Contexts.Minting' purpose specifically.

 @since 1.0.0
-}
data MintingBuilder (redeemer :: Type) = MB
    { mbInner :: BaseBuilder redeemer
    , mbTokensInputs :: Acc (SideUTXO Tokens)
    , mbTokensOutputs :: Acc (SideUTXO Tokens)
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

-- Ensures no coercion funny business
type role MintingBuilder nominal

-- | @since 1.0.0
instance Semigroup (MintingBuilder redeemer) where
    MB inner tins touts <> MB inner' tins' touts' =
        MB (inner <> inner') (tins <> tins') (touts <> touts')

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

{- | Describes a single input from a public key.

 @since 1.0.0
-}
inputFromPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Value ->
    MintingBuilder redeemer
inputFromPubKey pkh val = MB (Base.inputFromPubKey pkh val) mempty mempty

{- | As 'inputFromPubKey', but with some additional data.

 @since 1.0.0
-}
inputFromPubKeyWith ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Value ->
    a ->
    MintingBuilder redeemer
inputFromPubKeyWith pkh val x =
    MB (Base.inputFromPubKeyWith pkh val x) mempty mempty

{- | Describes a single input of 'Tokens' from a public key.

 @since 1.0.0
-}
inputTokensFromPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Tokens ->
    MintingBuilder redeemer
inputTokensFromPubKey pkh t = MB mempty (pure . sidePubKeyTokens pkh $ t) mempty

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
inputTokensFromPubKeyWith pkh t x =
    MB mempty (pure . sidePubKeyTokensWith pkh x $ t) mempty

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
inputFromOtherScript vh val x =
    MB (Base.inputFromOtherScript vh val x) mempty mempty

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
inputTokensFromOtherScript vh t x =
    MB mempty (pure . otherScriptTokens vh x $ t) mempty

{- | Describes a single output to a public key.

 @since 1.0.0
-}
outputToPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Value ->
    MintingBuilder redeemer
outputToPubKey pkh val = MB (Base.outputToPubKey pkh val) mempty mempty

{- | Describes a single output of 'Tokens' to a public key.

 @since 1.0.0
-}
outputTokensToPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Tokens ->
    MintingBuilder redeemer
outputTokensToPubKey pkh =
    MB mempty mempty . pure . sidePubKeyTokens pkh

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
outputToPubKeyWith pkh val x =
    MB (Base.outputToPubKeyWith pkh val x) mempty mempty

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
outputTokensToPubKeyWith pkh t x =
    MB mempty mempty . pure . sidePubKeyTokensWith pkh x $ t

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
outputToOtherScript vh val x =
    MB (Base.outputToOtherScript vh val x) mempty mempty

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
outputTokensToOtherScript vh t x =
    MB mempty mempty . pure . otherScriptTokens vh x $ t

{- | Describe that we've signed with one signature.

 @since 1.0.0
-}
signedWith ::
    forall (redeemer :: Type).
    PubKeyHash ->
    MintingBuilder redeemer
signedWith pkh = MB (Base.signedWith pkh) mempty mempty

{- | Describe a mint of tokens /not/ controlled by the minting policy for which
 this script context is being made.

 @since 1.0.0
-}
mintForeign ::
    forall (redeemer :: Type).
    Value ->
    MintingBuilder redeemer
mintForeign value = MB (Base.mint value) mempty mempty

{- | Describe that there is an additional piece of data in the context.

 @since 1.0.0
-}
extraData ::
    forall (a :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    MintingBuilder redeemer
extraData x = MB (Base.extraData x) mempty mempty

{- | Construct a 'ScriptContext' for minting, given a configuration and a list
 of things to do.

 @since 1.0.0
-}
mintingContext ::
    forall (redeemer :: Type).
    ContextConfig ->
    MintingBuilder redeemer ->
    NonEmpty MintingAction ->
    Maybe ScriptContext
mintingContext conf builder acts =
    ScriptContext <$> go <*> (pure . Minting $ ourSym)
  where
    ourSym :: CurrencySymbol
    ourSym = configCurrencySymbol conf
    ourAddress :: Address
    ourAddress = scriptHashAddress . configValidatorHash $ conf
    go :: Maybe TxInfo
    go = do
        baseTxInfo <- Base.compileBaseTxInfo conf . mbInner $ builder
        ourTokenInsList <-
            toList <$> (foldM checkSideAndCombine mempty . mbTokensInputs $ builder)
        ourTokenOutsList <-
            toList <$> (foldM checkSideAndCombine mempty . mbTokensOutputs $ builder)
        pure $
            baseTxInfo
                { txInfoInputs = txInfoInputs baseTxInfo <> createTxInInfos ourTokenInsList
                , txInfoOutputs =
                    txInfoOutputs baseTxInfo <> fmap (sideToTxOut ourSym) ourTokenOutsList
                , txInfoMint = mintingValue <> txInfoMint baseTxInfo
                , txInfoData =
                    txInfoData baseTxInfo
                        <> mapMaybe sideUTXOToDatum ourTokenInsList
                        <> mapMaybe sideUTXOToDatum ourTokenOutsList
                }
    mintingValue :: Value
    mintingValue = foldMap (processAction ourSym) acts
    checkSideAndCombine ::
        Acc (SideUTXO Tokens) ->
        SideUTXO Tokens ->
        Maybe (Acc (SideUTXO Tokens))
    checkSideAndCombine acc side@(SideUTXO typ _) = do
        let sideAddress = case typ of
                PubKeyUTXO pkh _ -> pubKeyHashAddress pkh
                ScriptUTXO vh _ -> scriptHashAddress vh
        Acc.cons <$> (guard (sideAddress /= ourAddress) $> side) <*> pure acc
    createTxInInfos :: [SideUTXO Tokens] -> [TxInInfo]
    createTxInInfos xs =
        let outRefs = TxOutRef (configTxId conf) <$> [1 ..]
         in zipWith TxInInfo outRefs . fmap (sideToTxOut ourSym) $ xs

-- Helpers

sideUTXOToDatum :: SideUTXO Tokens -> Maybe (DatumHash, Datum)
sideUTXOToDatum (SideUTXO typ _) = case typ of
    PubKeyUTXO _ mDat -> Base.datumWithHash <$> mDat
    ScriptUTXO _ dat -> Just . Base.datumWithHash $ dat

sideToTxOut :: CurrencySymbol -> SideUTXO Tokens -> TxOut
sideToTxOut sym (SideUTXO typ (T name amount)) =
    let value = Value.singleton sym name amount
     in case typ of
            PubKeyUTXO pkh mDat ->
                TxOut (pubKeyHashAddress pkh) value $ datumHash . dataToDatum <$> mDat
            ScriptUTXO vh dat ->
                TxOut (scriptHashAddress vh) value . Just . datumHash . dataToDatum $ dat

processAction :: CurrencySymbol -> MintingAction -> Value
processAction sym =
    uncurry (Value.singleton sym) . \case
        Mint (Tokens name amount) -> (name, amount)
        Burn (Tokens name amount) -> (name, negate amount)

sidePubKeyTokens :: PubKeyHash -> Tokens -> SideUTXO Tokens
sidePubKeyTokens pkh = SideUTXO (PubKeyUTXO pkh Nothing)

sidePubKeyTokensWith ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    a ->
    Tokens ->
    SideUTXO Tokens
sidePubKeyTokensWith pkh x =
    SideUTXO (PubKeyUTXO pkh . Just . Base.datafy $ x)

otherScriptTokens ::
    forall (a :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    a ->
    Tokens ->
    SideUTXO Tokens
otherScriptTokens vh x =
    SideUTXO (ScriptUTXO vh . Base.datafy $ x)

dataToDatum :: Data -> Datum
dataToDatum = Datum . BuiltinData
