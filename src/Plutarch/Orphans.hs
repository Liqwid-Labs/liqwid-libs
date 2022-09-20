{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module: Plutarch.Orphans
 Description: Orphan instances for Plutarch and Plutus types, including
  JSON serialization.
-}
module Plutarch.Orphans (AsBase16Bytes (AsBase16Bytes, unAsBase16Bytes)) where

import Control.Composition (on, (.*))
import Data.Coerce (Coercible, coerce)
import Plutarch.Api.V2 (PDatumHash (PDatumHash), PScriptHash (PScriptHash))
import Plutarch.Builtin (PIsData (..))
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.TryFrom (PTryFrom (..))
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

import qualified Codec.Serialise as Codec
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding

--------------------------------------------------------------------------------

import qualified PlutusLedgerApi.V1.Bytes as Plutus.Bytes
import qualified PlutusLedgerApi.V1.Scripts as Plutus.Scripts
import qualified PlutusLedgerApi.V2 as Plutus

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

-- | @since 1.3.0
instance
    {-# OVERLAPPABLE #-}
    (Semigroup (Term s a), a ~ PInner b) =>
    Semigroup (Term s b)
    where
    (<>) = punsafeDowncast .* ((<>) `on` punsafeCoerce)

-- | @since 1.3.0
instance
    {-# OVERLAPPABLE #-}
    (Monoid (Term s a), a ~ PInner b) =>
    Monoid (Term s b)
    where
    mempty = punsafeDowncast mempty

-- | @since 3.0.3
instance (PIsData a) => PIsData (PAsData a) where
    pfromDataImpl = punsafeCoerce
    pdataImpl = pdataImpl . pfromData

-- | @since 3.0.3
instance PTryFrom PData (PAsData PDatumHash) where
    type PTryFromExcess PData (PAsData PDatumHash) = Flip Term PDatumHash
    ptryFrom' opq = runTermCont $ do
        unwrapped <- pfromData . fst <$> ptryFromC @(PAsData PByteString) opq

        tcont $ \f ->
            pif
                -- Blake2b_256 hash: 256 bits/32 bytes.
                (plengthBS # unwrapped #== 32)
                (f ())
                (ptraceError "ptryFrom(PDatumHash): must be 32 bytes long")

        pure (punsafeCoerce opq, pcon $ PDatumHash unwrapped)

-- | @since 3.0.3
instance PTryFrom PData (PAsData PUnit)

-- | @since 3.0.3
instance PTryFrom PData (PAsData PScriptHash) where
    type PTryFromExcess PData (PAsData PScriptHash) = Flip Term PScriptHash
    ptryFrom' opq = runTermCont $ do
        unwrapped <- pfromData . fst <$> ptryFromC @(PAsData PByteString) opq

        tcont $ \f ->
            pif
                -- Blake2b_224 hash: 224 bits/28 bytes.
                (plengthBS # unwrapped #== 28)
                (f ())
                (ptraceError "ptryFrom(PScriptHash): must be 28 bytes long")

        pure (punsafeCoerce opq, pcon $ PScriptHash unwrapped)

----------------------------------------
-- Aeson (JSON) instances

-- | Represent a ByteString as a hex-encoded JSON String
newtype AsBase16Bytes (a :: Type) = AsBase16Bytes {unAsBase16Bytes :: a}

{- | Represent any serializable value as a hex-encoded JSON String of its
 serialization
-}
newtype AsBase16Codec (a :: Type) = AsBase16Codec {unAsBase16Codec :: a}

--------------------
-- Instances for `deriving via`

instance
    (Coercible a Plutus.LedgerBytes) =>
    Aeson.ToJSON (AsBase16Bytes a)
    where
    toJSON =
        Aeson.String
            . Plutus.Bytes.encodeByteString
            . Plutus.Bytes.bytes
            . coerce @(AsBase16Bytes a) @Plutus.Bytes.LedgerBytes

instance
    (Coercible Plutus.Bytes.LedgerBytes a) =>
    Aeson.FromJSON (AsBase16Bytes a)
    where
    parseJSON v =
        Aeson.parseJSON @T.Text v
            >>= either
                (Aeson.Types.parserThrowError [] . show)
                ( pure
                    . coerce @_
                        @(AsBase16Bytes a)
                )
                . Plutus.Bytes.fromHex
                . T.Encoding.encodeUtf8

instance (Codec.Serialise a) => Aeson.ToJSON (AsBase16Codec a) where
    toJSON =
        Aeson.String
            . Plutus.Bytes.encodeByteString
            . Lazy.toStrict
            . Codec.serialise @a
            . unAsBase16Codec

instance (Codec.Serialise a) => Aeson.FromJSON (AsBase16Codec a) where
    parseJSON v =
        Aeson.parseJSON @T.Text v
            >>= either
                (Aeson.Types.parserThrowError [] . show)
                (pure . AsBase16Codec)
                . Codec.deserialiseOrFail
                . Lazy.fromStrict
                . T.Encoding.encodeUtf8

deriving via (AsBase16Bytes Plutus.TxId) instance Aeson.ToJSON Plutus.TxId
deriving via (AsBase16Bytes Plutus.TxId) instance Aeson.FromJSON Plutus.TxId

deriving anyclass instance Aeson.ToJSON Plutus.TxOutRef
deriving anyclass instance Aeson.FromJSON Plutus.TxOutRef

deriving via
    (AsBase16Bytes Plutus.CurrencySymbol)
    instance
        (Aeson.ToJSON Plutus.CurrencySymbol)
deriving via
    (AsBase16Bytes Plutus.CurrencySymbol)
    instance
        (Aeson.FromJSON Plutus.CurrencySymbol)

deriving via
    (AsBase16Bytes Plutus.TokenName)
    instance
        (Aeson.ToJSON Plutus.TokenName)
deriving via
    (AsBase16Bytes Plutus.TokenName)
    instance
        (Aeson.FromJSON Plutus.TokenName)

deriving via
    (AsBase16Bytes Plutus.ValidatorHash)
    instance
        (Aeson.ToJSON Plutus.ValidatorHash)
deriving via
    (AsBase16Bytes Plutus.ValidatorHash)
    instance
        (Aeson.FromJSON Plutus.ValidatorHash)

deriving via
    (AsBase16Bytes Plutus.StakeValidatorHash)
    instance
        (Aeson.ToJSON Plutus.StakeValidatorHash)
deriving via
    (AsBase16Bytes Plutus.StakeValidatorHash)
    instance
        (Aeson.FromJSON Plutus.StakeValidatorHash)

deriving via
    (AsBase16Codec Plutus.StakeValidator)
    instance
        (Aeson.ToJSON Plutus.StakeValidator)
deriving via
    (AsBase16Codec Plutus.StakeValidator)
    instance
        (Aeson.FromJSON Plutus.StakeValidator)

deriving via
    (AsBase16Bytes Plutus.ScriptHash)
    instance
        (Aeson.ToJSON Plutus.ScriptHash)
deriving via
    (AsBase16Bytes Plutus.ScriptHash)
    instance
        (Aeson.FromJSON Plutus.ScriptHash)

deriving via
    Integer
    instance
        (Aeson.ToJSON Plutus.POSIXTime)
deriving via
    Integer
    instance
        (Aeson.FromJSON Plutus.POSIXTime)

deriving via
    (AsBase16Bytes Plutus.BuiltinByteString)
    instance
        (Aeson.ToJSON Plutus.BuiltinByteString)
deriving via
    (AsBase16Bytes Plutus.BuiltinByteString)
    instance
        (Aeson.FromJSON Plutus.BuiltinByteString)

deriving via
    (AsBase16Codec Plutus.Validator)
    instance
        (Aeson.ToJSON Plutus.Validator)
deriving via
    (AsBase16Codec Plutus.Validator)
    instance
        (Aeson.FromJSON Plutus.Validator)

deriving via
    (AsBase16Codec Plutus.MintingPolicy)
    instance
        (Aeson.ToJSON Plutus.MintingPolicy)
deriving via
    (AsBase16Codec Plutus.MintingPolicy)
    instance
        (Aeson.FromJSON Plutus.MintingPolicy)

deriving via
    (AsBase16Codec Plutus.Script)
    instance
        (Aeson.ToJSON Plutus.Script)
deriving via
    (AsBase16Codec Plutus.Script)
    instance
        (Aeson.FromJSON Plutus.Script)
