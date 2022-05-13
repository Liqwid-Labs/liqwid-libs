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

    -- ** Describe outputs
    outputValueToPubKey,
    outputTokensToPubKey,
    outputToPubKeyWith,
    outputTokensToPubKeyWith,
    outputToOtherScript,
    outputTokensToOtherScript,
) where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Kind (Type)
import Plutarch (S)
import Plutarch.Builtin (PIsData, pdata, pforgetData)
import Plutarch.Context.Internal (
    MintingBuilder (MB),
    SideUTXO (SideUTXO),
    UTXOType (PubKeyUTXO, ScriptUTXO),
    ValueType (GeneralValue, TokensValue),
 )
import Plutarch.Lift (PUnsafeLiftDecl (PLifted), pconstant, plift)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (TokenName, Value)
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

{- | Describes a single output to a 'PubKey'.

 @since 1.0.0
-}
outputValueToPubKey ::
    forall (redeemer :: Type).
    PubKeyHash ->
    Value ->
    MintingBuilder redeemer
outputValueToPubKey pkh val = output go
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

{- | As 'outputValueToPubKey', but adds some extra data as well.

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

-- Helpers

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

{-
-- | Describe a single input from a 'SideUTXO'.
--
-- @since 1.0.0
input :: forall (redeemer :: Type) .
  SideUTXO ->
  MintingBuilder redeemer
input x = MB (pure x) mempty mempty mempty mempty

-- | Describe a single output to a 'SideUTXO'.
--
-- @since 1.0.0

-- | Describe that we've signed with one signature.
--
-- @since 1.0.0
signedWith :: forall (redeemer :: Type) .
  PubKeyHash ->
  MintingBuilder redeemer
signedWith pkh = MB mempty mempty (pure pkh) mempty mempty

-- | Describe that there is an additional piece of data in the context.
--
-- @since 1.0.0
extraData :: forall (a :: Type) (redeemer :: Type) (p :: S -> Type) .
  (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
  a ->
  MintingBuilder redeemer
extraData x = MB mempty mempty mempty (pure . datafy $ x) mempty

-- | Describe that something other than the script we're using this context with
-- mints a value.
--
-- @since 1.0.0
otherMint :: forall (redeemer :: Type) .
  Minting ->
  MintingBuilder redeemer
otherMint m = MB mempty mempty mempty mempty (pure m)
-}
