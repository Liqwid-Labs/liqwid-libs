{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- The whole point of this module
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module: Plutarch.Orphans
 Description: Orphan instances for Plutarch and Plutus types, including
  JSON serialization.
-}
module Plutarch.Orphans () where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parserThrowError)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Coerce (Coercible, coerce)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Plutarch.Api.V2 (PDatumHash (PDatumHash), PScriptHash (PScriptHash))
import Plutarch.Builtin (PIsData (pdataImpl, pfromDataImpl))
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.TryFrom (PTryFrom (ptryFrom'), PTryFromExcess)
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1.Bytes (bytes, encodeByteString, fromHex)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  BuiltinData (BuiltinData),
  CurrencySymbol (CurrencySymbol),
  Data (I, List),
  LedgerBytes (LedgerBytes),
  MintingPolicy (MintingPolicy),
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
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
import PlutusTx (FromData (fromBuiltinData), ToData (toBuiltinData))

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

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
-- Instances for Ratios

instance ToData (Ratio Integer) where
  toBuiltinData rat =
    BuiltinData $
      List
        [ I $ numerator rat
        , I $ denominator rat
        ]

instance FromData (Ratio Integer) where
  fromBuiltinData (BuiltinData (List [I num, I denom])) =
    pure $ num % if num == 0 then 1 else denom
  fromBuiltinData _ = Nothing

----------------------------------------
-- Aeson (JSON) instances

-- | Represent a ByteString as a hex-encoded JSON String
newtype AsBase16Bytes (a :: Type) = AsBase16Bytes a

{- | Represent any serializable value as a hex-encoded JSON String of its
 serialization
-}
newtype AsBase16Codec (a :: Type) = AsBase16Codec a

--------------------
-- Instances for `deriving via`

-- @ since 3.6.1
instance
  (Coercible a LedgerBytes) =>
  Aeson.ToJSON (AsBase16Bytes a)
  where
  toJSON =
    Aeson.String
      . encodeByteString
      . bytes
      . coerce @(AsBase16Bytes a) @LedgerBytes

-- @ since 3.6.1
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

-- @ since 3.6.1
instance (Serialise a) => Aeson.ToJSON (AsBase16Codec a) where
  toJSON (AsBase16Codec x) =
    Aeson.String
      . encodeByteString
      . toStrict
      . serialise @a
      $ x

-- @ since 3.6.1
instance (Serialise a) => Aeson.FromJSON (AsBase16Codec a) where
  parseJSON v =
    Aeson.parseJSON @Text v
      >>= either
        (parserThrowError [] . show)
        (pure . AsBase16Codec)
        . deserialiseOrFail
        . fromStrict
        . encodeUtf8

-- @ since 3.6.1
deriving via (AsBase16Bytes TxId) instance Aeson.ToJSON TxId

-- @ since 3.6.1
deriving via (AsBase16Bytes TxId) instance Aeson.FromJSON TxId

-- @ since 3.6.1
deriving anyclass instance Aeson.ToJSON TxOutRef

-- @ since 3.6.1
deriving anyclass instance Aeson.FromJSON TxOutRef

-- @ since 3.6.1
deriving via
  (AsBase16Bytes CurrencySymbol)
  instance
    (Aeson.ToJSON CurrencySymbol)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes CurrencySymbol)
  instance
    (Aeson.FromJSON CurrencySymbol)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes TokenName)
  instance
    (Aeson.ToJSON TokenName)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes TokenName)
  instance
    (Aeson.FromJSON TokenName)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes ValidatorHash)
  instance
    (Aeson.ToJSON ValidatorHash)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes ValidatorHash)
  instance
    (Aeson.FromJSON ValidatorHash)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes StakeValidatorHash)
  instance
    (Aeson.ToJSON StakeValidatorHash)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes StakeValidatorHash)
  instance
    (Aeson.FromJSON StakeValidatorHash)

-- @ since 3.6.1
deriving via
  (AsBase16Codec StakeValidator)
  instance
    (Aeson.ToJSON StakeValidator)

-- @ since 3.6.1
deriving via
  (AsBase16Codec StakeValidator)
  instance
    (Aeson.FromJSON StakeValidator)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes ScriptHash)
  instance
    (Aeson.ToJSON ScriptHash)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes ScriptHash)
  instance
    (Aeson.FromJSON ScriptHash)

-- @ since 3.6.1
deriving via
  Integer
  instance
    (Aeson.ToJSON POSIXTime)

-- @ since 3.6.1
deriving via
  Integer
  instance
    (Aeson.FromJSON POSIXTime)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes BuiltinByteString)
  instance
    (Aeson.ToJSON BuiltinByteString)

-- @ since 3.6.1
deriving via
  (AsBase16Bytes BuiltinByteString)
  instance
    (Aeson.FromJSON BuiltinByteString)

-- @ since 3.6.1
deriving via
  (AsBase16Codec Validator)
  instance
    (Aeson.ToJSON Validator)

-- @ since 3.6.1
deriving via
  (AsBase16Codec Validator)
  instance
    (Aeson.FromJSON Validator)

-- @ since 3.6.1
deriving via
  (AsBase16Codec MintingPolicy)
  instance
    (Aeson.ToJSON MintingPolicy)

-- @ since 3.6.1
deriving via
  (AsBase16Codec MintingPolicy)
  instance
    (Aeson.FromJSON MintingPolicy)

-- @ since 3.6.1
deriving via
  (AsBase16Codec Script)
  instance
    (Aeson.ToJSON Script)

-- @ since 3.6.1
deriving via
  (AsBase16Codec Script)
  instance
    (Aeson.FromJSON Script)

-- @ since 3.16.0
deriving via
  BuiltinByteString
  instance
    (Aeson.ToJSON PubKeyHash)

-- @ since 3.16.0
deriving via
  BuiltinByteString
  instance
    (Aeson.FromJSON PubKeyHash)
