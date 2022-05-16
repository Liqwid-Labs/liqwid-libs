{-# LANGUAGE TypeFamilies #-}

module Plutarch.Context.Spending (
    -- * Types
    SpendingBuilder,

    -- * Functions

    -- ** Describe inputs
    inputFromPubKey,
    inputFromPubKeyWith,
    inputFromOtherScript,
    inputSelfExtra,

    -- ** Describe outputs
    outputToPubKey,
    outputToPubKeyWith,
    outputToOtherScript,
    outputToValidator,

    -- ** Describe others
    mint,
    signedWith,
    extraData,
) where

import Data.Kind (Type)
import Plutarch (S)
import Plutarch.Builtin (PIsData)
import Plutarch.Context.Internal (
    MintingBuilder (MB),
    SpendingBuilder (SB),
    ValidatorUTXO (ValidatorUTXO),
 )
import qualified Plutarch.Context.Minting as MB
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (Value)

{- | Describes a single input from a 'PubKey'.

 @since 1.0.0
-}
inputFromPubKey ::
    forall (datum :: Type) (redeemer :: Type).
    PubKeyHash ->
    Value ->
    SpendingBuilder datum redeemer
inputFromPubKey pkh val = SB (MB.inputFromPubKey pkh val) mempty mempty

{- | As 'inputFromPubKey', but with some additional provided data.

 @since 1.0.0
-}
inputFromPubKeyWith ::
    forall (a :: Type) (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Value ->
    a ->
    SpendingBuilder datum redeemer
inputFromPubKeyWith pkh val x =
    SB (MB.inputFromPubKeyWith pkh val x) mempty mempty

{- | Describes an input from a script /other/ than the one for which the context
 is being made.

 @since 1.0.0
-}
inputFromOtherScript ::
    forall (a :: Type) (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    Value ->
    a ->
    SpendingBuilder datum redeemer
inputFromOtherScript vh val x =
    SB (MB.inputFromOtherScript vh val x) mempty mempty

{- | Describes an input given to the validator which we do /not/ want checked.

 @since 1.0.0
-}
inputSelfExtra ::
    forall (datum :: Type) (redeemer :: Type).
    Value ->
    datum ->
    SpendingBuilder datum redeemer
inputSelfExtra val x =
    SB (MB mempty mempty mempty mempty mempty) (pure go) mempty
  where
    go :: ValidatorUTXO datum
    go = ValidatorUTXO x val

{- | Describes a single output to a 'PubKey'.

 @since 1.0.0
-}
outputToPubKey ::
    forall (datum :: Type) (redeemer :: Type).
    PubKeyHash ->
    Value ->
    SpendingBuilder datum redeemer
outputToPubKey pkh val = SB (MB.outputToPubKey pkh val) mempty mempty

{- | As 'outputToPubKey', but with extra data.

 @since 1.0.0
-}
outputToPubKeyWith ::
    forall (a :: Type) (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    PubKeyHash ->
    Value ->
    a ->
    SpendingBuilder datum redeemer
outputToPubKeyWith pkh val x =
    SB (MB.outputToPubKeyWith pkh val x) mempty mempty

{- | Describes an output to a script /other/ than the one for which this context
 is being made.

 @since 1.0.0
-}
outputToOtherScript ::
    forall (a :: Type) (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    ValidatorHash ->
    Value ->
    a ->
    SpendingBuilder datum redeemer
outputToOtherScript vh val x =
    SB (MB.outputToOtherScript vh val x) mempty mempty

{- | Describes an output to the address of the script being validated.

 @since 1.0.0
-}
outputToValidator ::
    forall (datum :: Type) (redeemer :: Type).
    Value ->
    datum ->
    SpendingBuilder datum redeemer
outputToValidator val x =
    SB (MB mempty mempty mempty mempty mempty) mempty (pure go)
  where
    go :: ValidatorUTXO datum
    go = ValidatorUTXO x val

{- | Describes a mint.

 @since 1.0.0
-}
mint ::
    forall (datum :: Type) (redeemer :: Type).
    Value ->
    SpendingBuilder datum redeemer
mint val = SB (MB.mintForeign val) mempty mempty

{- | Describes that we've signed with one signature.

 @since 1.0.0
-}
signedWith ::
    forall (datum :: Type) (redeemer :: Type).
    PubKeyHash ->
    SpendingBuilder datum redeemer
signedWith pkh = SB (MB.signedWith pkh) mempty mempty

{- | Describes an aditional piece of data in the context.

 @since 1.0.0
-}
extraData ::
    forall (a :: Type) (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    SpendingBuilder datum redeemer
extraData x = SB (MB.extraData x) mempty mempty
