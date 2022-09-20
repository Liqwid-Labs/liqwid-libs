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
module Plutarch.Orphans (AsBase16Bytes (..)) where

import Control.Composition (on, (.*))
import Data.Coerce (Coercible, coerce)
import Plutarch.Api.V2 (PDatumHash (PDatumHash), PScriptHash (PScriptHash))
import Plutarch.Builtin (PIsData (pdataImpl, pfromDataImpl))
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.TryFrom (PTryFrom (ptryFrom'), PTryFromExcess)
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parserThrowError)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1.Bytes (bytes, encodeByteString, fromHex)
import PlutusLedgerApi.V2 (
    BuiltinByteString,
    CurrencySymbol (CurrencySymbol),
    LedgerBytes (LedgerBytes),
    MintingPolicy (MintingPolicy),
    POSIXTime (POSIXTime),
    Script,
    ScriptHash (ScriptHash),
    StakeValidator (StakeValidator),
    StakeValidatorHash (StakeValidatorHash),
    TokenName (TokenName),
    TxId (TxId),
    TxOutRef,
    Validator (Validator),
    ValidatorHash (ValidatorHash),
 )

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
    (Coercible a LedgerBytes) =>
    Aeson.ToJSON (AsBase16Bytes a)
    where
    toJSON =
        Aeson.String
            . encodeByteString
            . bytes
            . coerce @(AsBase16Bytes a) @LedgerBytes

instance
    (Coercible LedgerBytes a) =>
    Aeson.FromJSON (AsBase16Bytes a)
    where
    parseJSON v =
        Aeson.parseJSON @Text v
            >>= either
                (parserThrowError [] . show)
                ( pure
                    . coerce @_
                        @(AsBase16Bytes a)
                )
                . fromHex
                . encodeUtf8

instance (Serialise a) => Aeson.ToJSON (AsBase16Codec a) where
    toJSON =
        Aeson.String
            . encodeByteString
            . toStrict
            . serialise @a
            . unAsBase16Codec

instance (Serialise a) => Aeson.FromJSON (AsBase16Codec a) where
    parseJSON v =
        Aeson.parseJSON @Text v
            >>= either
                (parserThrowError [] . show)
                (pure . AsBase16Codec)
                . deserialiseOrFail
                . fromStrict
                . encodeUtf8

deriving via (AsBase16Bytes TxId) instance Aeson.ToJSON TxId
deriving via (AsBase16Bytes TxId) instance Aeson.FromJSON TxId

deriving anyclass instance Aeson.ToJSON TxOutRef
deriving anyclass instance Aeson.FromJSON TxOutRef

deriving via
    (AsBase16Bytes CurrencySymbol)
    instance
        (Aeson.ToJSON CurrencySymbol)
deriving via
    (AsBase16Bytes CurrencySymbol)
    instance
        (Aeson.FromJSON CurrencySymbol)

deriving via
    (AsBase16Bytes TokenName)
    instance
        (Aeson.ToJSON TokenName)
deriving via
    (AsBase16Bytes TokenName)
    instance
        (Aeson.FromJSON TokenName)

deriving via
    (AsBase16Bytes ValidatorHash)
    instance
        (Aeson.ToJSON ValidatorHash)
deriving via
    (AsBase16Bytes ValidatorHash)
    instance
        (Aeson.FromJSON ValidatorHash)

deriving via
    (AsBase16Bytes StakeValidatorHash)
    instance
        (Aeson.ToJSON StakeValidatorHash)
deriving via
    (AsBase16Bytes StakeValidatorHash)
    instance
        (Aeson.FromJSON StakeValidatorHash)

deriving via
    (AsBase16Codec StakeValidator)
    instance
        (Aeson.ToJSON StakeValidator)
deriving via
    (AsBase16Codec StakeValidator)
    instance
        (Aeson.FromJSON StakeValidator)

deriving via
    (AsBase16Bytes ScriptHash)
    instance
        (Aeson.ToJSON ScriptHash)
deriving via
    (AsBase16Bytes ScriptHash)
    instance
        (Aeson.FromJSON ScriptHash)

deriving via
    Integer
    instance
        (Aeson.ToJSON POSIXTime)
deriving via
    Integer
    instance
        (Aeson.FromJSON POSIXTime)

deriving via
    (AsBase16Bytes BuiltinByteString)
    instance
        (Aeson.ToJSON BuiltinByteString)
deriving via
    (AsBase16Bytes BuiltinByteString)
    instance
        (Aeson.FromJSON BuiltinByteString)

deriving via
    (AsBase16Codec Validator)
    instance
        (Aeson.ToJSON Validator)
deriving via
    (AsBase16Codec Validator)
    instance
        (Aeson.FromJSON Validator)

deriving via
    (AsBase16Codec MintingPolicy)
    instance
        (Aeson.ToJSON MintingPolicy)
deriving via
    (AsBase16Codec MintingPolicy)
    instance
        (Aeson.FromJSON MintingPolicy)

deriving via
    (AsBase16Codec Script)
    instance
        (Aeson.ToJSON Script)
deriving via
    (AsBase16Codec Script)
    instance
        (Aeson.FromJSON Script)
