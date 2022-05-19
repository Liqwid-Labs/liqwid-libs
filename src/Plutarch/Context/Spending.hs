{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- | Module: Plutarch.Context.Spending
 Copyright: (C) Liqwid Labs 2022
 License: Proprietary
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for spending contexts. 'SpendingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'SpendingBuilder' using '<>'.
-}
module Plutarch.Context.Spending (
    -- * Types
    SpendingBuilder,
    ValidatorUTXO (..),

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

    -- ** Compilation
    spendingContext,
) where

import Acc (Acc)
import Control.Monad (foldM)
import Control.Monad.Reader (asks)
import Data.Foldable (toList)
import Data.Kind (Type)
import Plutarch (S)
import Plutarch.Builtin (PIsData)
import Plutarch.Context.Base (BaseBuilder, Build)
import qualified Plutarch.Context.Base as Base
import Plutarch.Context.Config (
    ContextConfig (
        configTxId,
        configValidatorHash
    ),
 )
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Contexts (
    ScriptContext (ScriptContext),
    ScriptPurpose (Spending),
    TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
    TxInfo (
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories
    ),
    TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
    TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, ValidatorHash)
import Plutus.V1.Ledger.Value (Value)

{- | A context builder for spending. Corresponds broadly to validators, and to
 'Plutus.V1.Ledger.Contexts.Spending' specifically.

 @since 1.0.0
-}
data SpendingBuilder (datum :: Type) (redeemer :: Type) = SB
    { sbInner :: BaseBuilder redeemer
    , sbValidatorInputs :: Acc (ValidatorUTXO datum)
    , sbValidatorOutputs :: Acc (ValidatorUTXO datum)
    }
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

-- Avoids any coercion funny business
type role SpendingBuilder nominal nominal

-- | @since 1.0.0
instance Semigroup (SpendingBuilder datum redeemer) where
    SB inner ins outs <> SB inner' ins' outs' =
        SB (inner <> inner') (ins <> ins') (outs <> outs')

{- | UTxO at the validator's address. This represents an input that we /do/ want
 to validate.

 @since 1.0.0
-}
data ValidatorUTXO (datum :: Type) = ValidatorUTXO datum Value
    deriving stock
        ( -- | @since 1.0.0
          Show
        )

{- | Describes a single input from a public key.

 @since 1.0.0
-}
inputFromPubKey ::
    forall (datum :: Type) (redeemer :: Type).
    PubKeyHash ->
    Value ->
    SpendingBuilder datum redeemer
inputFromPubKey pkh val = SB (Base.inputFromPubKey pkh val) mempty mempty

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
    SB (Base.inputFromPubKeyWith pkh val x) mempty mempty

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
    SB (Base.inputFromOtherScript vh val x) mempty mempty

{- | Describes an input given to the validator which we do /not/ want checked.

 @since 1.0.0
-}
inputSelfExtra ::
    forall (datum :: Type) (redeemer :: Type).
    Value ->
    datum ->
    SpendingBuilder datum redeemer
inputSelfExtra val x =
    SB mempty (pure go) mempty
  where
    go :: ValidatorUTXO datum
    go = ValidatorUTXO x val

{- | Describes a single output to a public key.

 @since 1.0.0
-}
outputToPubKey ::
    forall (datum :: Type) (redeemer :: Type).
    PubKeyHash ->
    Value ->
    SpendingBuilder datum redeemer
outputToPubKey pkh val = SB (Base.outputToPubKey pkh val) mempty mempty

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
    SB (Base.outputToPubKeyWith pkh val x) mempty mempty

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
    SB (Base.outputToOtherScript vh val x) mempty mempty

{- | Describes an output to the address of the script being validated.

 @since 1.0.0
-}
outputToValidator ::
    forall (datum :: Type) (redeemer :: Type).
    Value ->
    datum ->
    SpendingBuilder datum redeemer
outputToValidator val x =
    SB mempty mempty (pure go)
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
mint val = SB (Base.mint val) mempty mempty

{- | Describes that we've signed with one signature.

 @since 1.0.0
-}
signedWith ::
    forall (datum :: Type) (redeemer :: Type).
    PubKeyHash ->
    SpendingBuilder datum redeemer
signedWith pkh = SB (Base.signedWith pkh) mempty mempty

{- | Describes an aditional piece of data in the context.

 @since 1.0.0
-}
extraData ::
    forall (a :: Type) (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ a, PIsData p) =>
    a ->
    SpendingBuilder datum redeemer
extraData x = SB (Base.extraData x) mempty mempty

{- | Construct a 'ScriptContext' for spending, given a configuration and a
 description of what inputs to the script should be checked.

 = Note

 As part of construction, we verify if any \'additional\' inputs have
 addresses different from that of the script. If an \'additional\' input's
 address matches the one in the configuration, this function will fail.

 @since 1.0.0
-}
spendingContext ::
    forall (datum :: Type) (redeemer :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ datum, PIsData p) =>
    ContextConfig ->
    SpendingBuilder datum redeemer ->
    ValidatorUTXO datum ->
    Maybe ScriptContext
spendingContext conf builder (ValidatorUTXO d v) = Base.runBuild go conf
  where
    go :: Build ScriptContext
    go = do
        tid <- asks configTxId
        let purpose = Spending . TxOutRef tid $ 0
        baseInfo <- Base.baseTxInfo
        let inner = sbInner builder
        (baseIn, baseInData) <-
            Base.toInInfoDatums id
                . Base.bbInputs
                $ inner
        let (baseOut, baseOutData) =
                Base.toTxOutDatums id
                    . Base.bbOutputs
                    $ inner
        value <- Base.combineExternalMints . Base.bbMints $ inner
        (vIn, vInData) <-
            foldM validatorInInfoDatums (mempty, mempty)
                . sbValidatorInputs
                $ builder
        vOut <- traverse validatorTxOut . sbValidatorOutputs $ builder
        inInfo <- createOwnInfo
        let inData = Base.datumWithHash . Base.datafy $ d
        let fullInfo =
                baseInfo
                    { txInfoMint = value
                    , txInfoInputs = (inInfo :) . toList $ baseIn <> vIn
                    , txInfoOutputs = toList $ baseOut <> vOut
                    , txInfoSignatories = toList . Base.bbSignatures $ inner
                    , txInfoData =
                        (inData :) . toList $
                            (Base.datumWithHash <$> Base.bbDatums inner)
                                <> baseInData
                                <> baseOutData
                                <> vInData
                    }
        pure . ScriptContext fullInfo $ purpose
    createOwnInfo :: Build TxInInfo
    createOwnInfo = do
        tid <- asks configTxId
        address <- asks (scriptHashAddress . configValidatorHash)
        pure $
            TxInInfo
                { txInInfoOutRef = TxOutRef tid 0
                , txInInfoResolved =
                    TxOut
                        { txOutAddress = address
                        , txOutValue = v
                        , txOutDatumHash = Just . fst . Base.datumWithHash . Base.datafy $ d
                        }
                }

-- Helpers

validatorTxOut ::
    forall (datum :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ datum, PIsData p) =>
    ValidatorUTXO datum ->
    Build TxOut
validatorTxOut (ValidatorUTXO d v) = do
    address <- asks (scriptHashAddress . configValidatorHash)
    pure $
        TxOut
            { txOutAddress = address
            , txOutValue = v
            , txOutDatumHash = Just . fst . Base.datumWithHash . Base.datafy $ d
            }

validatorInInfoDatums ::
    forall (datum :: Type) (p :: S -> Type).
    (PUnsafeLiftDecl p, PLifted p ~ datum, PIsData p) =>
    (Acc TxInInfo, Acc (DatumHash, Datum)) ->
    ValidatorUTXO datum ->
    Build (Acc TxInInfo, Acc (DatumHash, Datum))
validatorInInfoDatums (acc1, acc2) vutxo@(ValidatorUTXO d _) = do
    ref <- asks (TxOutRef . configTxId) <*> Base.freshOutRefIndex
    txOut <- validatorTxOut vutxo
    let txInInfo = TxInInfo ref txOut
    let vdata = Base.datumWithHash . Base.datafy $ d
    pure (pure txInInfo <> acc1, pure vdata <> acc2)
