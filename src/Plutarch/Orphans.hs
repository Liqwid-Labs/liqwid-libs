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
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parserThrowError)
import qualified Data.ByteString as ByteStringStrict
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Coerce (Coercible, coerce)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import Plutarch.Api.V2 (PDatumHash (PDatumHash), PScriptHash (PScriptHash))
import Plutarch.Builtin (PIsData (pdataImpl, pfromDataImpl))
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.TryFrom (PTryFrom (ptryFrom'), PTryFromExcess)
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------

import PlutusLedgerApi.V1.Bytes (bytes, encodeByteString, fromHex)
import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  BuiltinData (BuiltinData),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (CurrencySymbol, unCurrencySymbol),
  Data (I, List),
  Datum,
  LedgerBytes (LedgerBytes),
  MintingPolicy (MintingPolicy),
  POSIXTime (POSIXTime),
  PubKeyHash (PubKeyHash),
  Script,
  ScriptHash (ScriptHash),
  StakeValidator (StakeValidator),
  StakeValidatorHash (StakeValidatorHash),
  StakingCredential (StakingHash, StakingPtr),
  TokenName (TokenName),
  TxId (TxId),
  TxOutRef,
  Validator (Validator),
  ValidatorHash (ValidatorHash),
  fromBuiltin,
  toBuiltin,
 )
import PlutusTx (FromData (fromBuiltinData), ToData (toBuiltinData))

--------------------------------------------------------------------------------

tryDecode :: Text -> Either String ByteStringStrict.ByteString
tryDecode = Base16.decode . encodeUtf8

decodeByteString :: Aeson.Value -> Parser ByteStringStrict.ByteString
decodeByteString = Aeson.withText "ByteString" (either fail pure . tryDecode)

--------------------------------------------------------------------------------

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

  toEncoding =
    Aeson.toEncoding @Text
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

  toEncoding (AsBase16Codec x) =
    Aeson.toEncoding @Text
      . encodeByteString
      . toStrict
      . serialise @a
      $ x

-- @ since 3.6.1
instance (Serialise a) => Aeson.FromJSON (AsBase16Codec a) where
  parseJSON v = do
    eitherLedgerBytes <-
      fromHex . encodeUtf8
        <$> Aeson.parseJSON @Text v

    b <- case eitherLedgerBytes of
      (Left err) -> parserThrowError [] $ show err
      (Right lb) -> pure $ bytes lb
    case deserialiseOrFail (fromStrict b) of
      (Left err) -> parserThrowError [] $ show err
      (Right r) -> pure $ AsBase16Codec r

-- @since X.Y.Z
deriving via (AsBase16Codec Datum) instance Aeson.ToJSON Datum

-- @since X.Y.Z
deriving via (AsBase16Codec Datum) instance Aeson.FromJSON Datum

-- @ since 3.6.1
deriving via (AsBase16Bytes TxId) instance Aeson.ToJSON TxId

-- @ since 3.6.1
deriving via (AsBase16Bytes TxId) instance Aeson.FromJSON TxId

-- @ since 3.6.1
deriving anyclass instance Aeson.ToJSON TxOutRef

-- @ since 3.6.1
deriving anyclass instance Aeson.FromJSON TxOutRef

-- @ since 3.19.3
instance Aeson.ToJSON CurrencySymbol where
  toJSON c =
    Aeson.object
      [
        ( "unCurrencySymbol"
        , Aeson.String
            . encodeByteString
            . fromBuiltin
            . unCurrencySymbol
            $ c
        )
      ]

-- @ since 3.19.3
instance Aeson.FromJSON CurrencySymbol where
  parseJSON =
    Aeson.withObject "CurrencySymbol" $ \object -> do
      raw <- object .: "unCurrencySymbol"
      bytes' <- decodeByteString raw
      pure $ CurrencySymbol $ toBuiltin bytes'

{- Copied from an old version of Plutarch:
https://github.com/input-output-hk/plutus/blob/4fd86930f1dc628a816adf5f5d854b3fec578312/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs#L155

note [Roundtripping token names]

How to properly roundtrip a token name that is not valid UTF-8 through PureScript
without a big rewrite of the API?
We prefix it with a zero byte so we can recognize it when we get a bytestring value back,
and we serialize it base16 encoded, with 0x in front so it will look as a hex string.
(Browsers don't render the zero byte.)
-}

-- @ since 3.19.3
instance Aeson.ToJSON TokenName where
  toJSON =
    Aeson.object
      . pure
      . (,) "unTokenName"
      . Aeson.toJSON
      . fromTokenName
        (Text.cons '\NUL' . asBase16)
        ( \t -> case Text.take 1 t of
            "\NUL" -> Text.concat ["\NUL\NUL", t]
            _ -> t
        )
    where
      fromTokenName ::
        (ByteStringStrict.ByteString -> r) ->
        (Text -> r) ->
        TokenName ->
        r
      fromTokenName handleBytestring handleText (TokenName bs) =
        either (\_ -> handleBytestring $ fromBuiltin bs) handleText $
          Text.Encoding.decodeUtf8' (fromBuiltin bs)

      asBase16 :: ByteStringStrict.ByteString -> Text
      asBase16 bs = Text.concat ["0x", encodeByteString bs]

-- @ since 3.19.3
instance Aeson.FromJSON TokenName where
  parseJSON =
    Aeson.withObject "TokenName" $ \object -> do
      raw <- object .: "unTokenName"
      fromJSONText raw
    where
      fromJSONText t = case Text.take 3 t of
        "\NUL0x" -> either fail (pure . tokenName) . tryDecode . Text.drop 3 $ t
        "\NUL\NUL\NUL" ->
          pure . tokenName . Text.Encoding.encodeUtf8 . Text.drop 2 $ t
        _ -> pure . tokenName . Text.Encoding.encodeUtf8 $ t

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

--------------------------------------------------------------------------------
-- manual instances

-- @ since 3.16.0
instance Aeson.ToJSON StakingCredential where
  toJSON (StakingHash cred) =
    Aeson.object
      [ "contents" .= Aeson.toJSON cred
      , "tag" .= Aeson.String "StakingHash"
      ]
  toJSON (StakingPtr x y z) =
    Aeson.object
      [ "contents"
          .= Aeson.Array
            ( Vector.fromList
                (Aeson.toJSON <$> [x, y, z])
            )
      , "tag" .= Aeson.String "StakingPtr"
      ]

  toEncoding (StakingHash cred) =
    Aeson.pairs
      ( "contents"
          .= cred
          <> "tag"
          .= Aeson.String "StakingHash"
      )
  toEncoding (StakingPtr x y z) =
    Aeson.pairs
      ( "contents"
          .= [x, y, z]
          <> "tag"
          .= Aeson.String "StakingPtr"
      )

-- @since 3.16.0
instance Aeson.FromJSON StakingCredential where
  parseJSON = Aeson.withObject "StakingCredential" $ \v -> do
    contents <- v .: "contents"
    tag <- v .: "tag"
    case tag of
      "StakingHash" -> StakingHash <$> Aeson.parseJSON contents
      "StakingPtr" -> parseStakingPtr contents
      _ -> fail $ "Expected StakingHash or StakingPtr, got " <> tag
    where
      parseStakingPtr :: Aeson.Value -> Parser StakingCredential
      parseStakingPtr v =
        Aeson.parseJSONList v >>= \case
          [x, y, z] -> pure $ StakingPtr x y z
          xs ->
            fail $
              "expected an array of length 3, but got length "
                <> show (length xs)

-- @since 3.16.0
instance Aeson.ToJSON Credential where
  toJSON (PubKeyCredential cred) =
    Aeson.object
      [ "contents" .= Aeson.toJSON cred
      , "tag" .= Aeson.String "PubKeyCredential"
      ]
  toJSON (ScriptCredential cred) =
    Aeson.object
      [ "contents" .= Aeson.toJSON cred
      , "tag" .= Aeson.String "ScriptCredential"
      ]

  toEncoding (PubKeyCredential cred) =
    Aeson.pairs
      ( "contents"
          .= cred
          <> "tag"
          .= Aeson.String "PubKeyCredential"
      )
  toEncoding (ScriptCredential cred) =
    Aeson.pairs
      ( "contents"
          .= cred
          <> "tag"
          .= Aeson.String "ScriptCredential"
      )

-- @since 3.16.0
instance Aeson.FromJSON Credential where
  parseJSON = Aeson.withObject "Credential" $ \v -> do
    contents <- v .: "contents"
    tag <- v .: "tag"
    case tag of
      "PubKeyCredential" -> PubKeyCredential <$> Aeson.parseJSON contents
      "ScriptCredential" -> ScriptCredential <$> Aeson.parseJSON contents
      _ -> fail $ "Expected PubKeyCredential or ScriptCredential, got " <> tag
