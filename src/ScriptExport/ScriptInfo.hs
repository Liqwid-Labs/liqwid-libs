{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : Agora.ScriptInfo
Maintainer : emi@haskell.fyi
Description: Exportable script bundles for off-chain consumption.

Exportable script bundles for off-chain consumption.
-}
module ScriptExport.ScriptInfo (
  -- * Types
  ScriptInfo (..),
  Linker (..),
  RawScriptExport (..),
  ScriptExport (..),

  -- * Linker utilities
  runLinker,
  fetchTS,
  exportParam,
  exportVersion,

  -- * Introduction functions
  mkScriptInfo,
  mkValidatorInfo,
  mkPolicyInfo,
  mkStakeValidatorInfo,
) where

import Plutarch.Orphans ()
import Cardano.Binary qualified as CBOR
import Codec.Serialise qualified as Codec
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Kind (Type)
import Data.Map (Map, (!?))
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as Text
import GHC.Generics qualified as GHC
import Optics (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch (
  ClosedTerm,
  Config (Config, tracingMode),
  TracingMode (NoTracing),
 )
import Plutarch.Api.V2 (
  PMintingPolicy,
  PStakeValidator,
  PValidator,
  mkMintingPolicy,
  mkStakeValidator,
  mkValidator,
  scriptHash,
 )
import PlutusLedgerApi.V2 (
  MintingPolicy (getMintingPolicy),
  Script,
  StakeValidator (getStakeValidator),
  Validator (getValidator),
 )
import Ply (ScriptRole, TypedScript, TypedScriptEnvelope)
import Ply.Core.TypedReader (TypedReader, mkTypedScript)

-- | @since 2.0.0
data LinkerError
  = VersionMismatch
  | ScriptNotFound Text
  | ScriptTypeMismatch Text
  | Other Text
  deriving stock
    ( -- | @since 2.0.0
      Show
    , -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      Aeson.ToJSON
    , -- | @since 2.0.0
      Aeson.FromJSON
    )

{- | Type for linker that makes ready-to-go script from parameter and
     raw script.

 @since 2.0.0
-}
newtype Linker param a
  = Linker (ReaderT (RawScriptExport, param) (Except LinkerError) a)
  deriving
    ( -- | @since 2.0.0
      Functor
    , -- | @since 2.0.0
      Applicative
    , -- | @since 2.0.0
      Monad
    , -- | @since 2.0.0
      MonadError LinkerError
    , -- | @since 2.0.0
      MonadReader (RawScriptExport, param)
    )
    via (ReaderT (RawScriptExport, param) (Except LinkerError))

{- | Run linker with parameter and raw scripts.

 @since 2.0.0
-}
runLinker ::
  forall (param :: Type) (a :: Type).
  Linker param (ScriptExport a) ->
  RawScriptExport ->
  param ->
  Either LinkerError (ScriptExport a)
runLinker (Linker linker) rs p =
  runExcept $ runReaderT linker (rs, p)

{- | Type for holding parameterized scripts. This type represents the
     raw scripts which needs to be linked in order to be deployed.

 @since 2.0.0
-}
data RawScriptExport = RawScriptExport
  { version :: String
  , scripts :: Map Text TypedScriptEnvelope
  }
  deriving stock
    ( -- | @since 2.0.0
      Show
    , -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      Aeson.ToJSON
    , -- | @since 2.0.0
      Aeson.FromJSON
    )

-- | @since 2.0.0
fetchTS ::
  forall (rl :: ScriptRole) (params :: [Type]) (lparam :: Type).
  TypedReader rl params =>
  Text ->
  Linker lparam (TypedScript rl params)
fetchTS t = do
  (scr, _) <- ask
  case view #scripts scr !? t of
    Just scr' ->
      case mkTypedScript @rl @params scr' of
        Right x -> return x
        Left err -> throwError $ ScriptTypeMismatch $ pack $ show err
    Nothing -> throwError $ ScriptNotFound t

-- | @since 2.0.0
exportParam :: forall (lparam :: Type). Linker lparam lparam
exportParam = asks snd

-- | @since 2.0.0
exportVersion :: forall (lparam :: Type). Linker lparam String
exportVersion = asks (view #version . fst)

{- | Type for holding ready-to-go scripts. Ready-to-go scripts are
     scripts that can be used on-onchain without any extra
     arguments. They are `MintingPolicy ~ StakingValidator ~ PData ->
     PScriptContext -> POpaque` and `Validator ~ PData -> PData ->
     PScriptContext -> POpaque`.

 @since 2.0.0
-}
data ScriptExport a = ScriptExport
  { version :: String
  , scripts :: Map Text Script
  , information :: a
  }
  deriving stock
    ( -- | @since 2.0.0
      Show
    , -- | @since 2.0.0
      Eq
    , -- | @since 2.0.0
      GHC.Generic
    )

-- Internal helper for CBOR deserialization
newtype CBORSerializedScript = CBORSerializedScript SBS.ShortByteString

instance CBOR.FromCBOR CBORSerializedScript where
  fromCBOR = CBORSerializedScript . SBS.toShort <$> CBOR.fromCBOR

cborToScript :: BS.ByteString -> Either CBOR.DecoderError Script
cborToScript x =
  Codec.deserialise
    . LBS.fromStrict
    . SBS.fromShort
    . (\(CBORSerializedScript x) -> x)
    <$> CBOR.decodeFull' x

-- | @since 2.0.0
instance
  forall (a :: Type).
  Aeson.ToJSON a =>
  Aeson.ToJSON (ScriptExport a)
  where
  toJSON (ScriptExport version scripts info) =
    Aeson.toJSON $
      Aeson.object
        [ "version" Aeson..= version
        , "info" Aeson..= info
        , "scripts" Aeson..= (toJSONScript <$> scripts)
        ]
    where
      toJSONScript scr =
        let raw = LBS.toStrict $ Codec.serialise scr
            cbor = CBOR.serialize' $ SBS.toShort raw
         in Aeson.toJSON $
              Aeson.object
                [ "cborHex" Aeson..= Base16.encodeBase16 cbor
                , "rawHex" Aeson..= Base16.encodeBase16 raw
                , "hash" Aeson..= scriptHash scr
                ]

-- | @since 2.0.0
instance
  forall (a :: Type).
  Aeson.FromJSON a =>
  Aeson.FromJSON (ScriptExport a)
  where
  parseJSON (Aeson.Object v) =
    ScriptExport
      <$> v Aeson..: "version"
      <*> (v Aeson..: "scripts" >>= Aeson.liftParseJSON parseJSONScriptMap Aeson.parseJSON)
      <*> v Aeson..: "info"
    where
      parseAndDeserialize v =
        Aeson.parseJSON v
          >>= either (fail . show) (either (fail . show) pure . cborToScript)
            . Base16.decodeBase16
            . Text.encodeUtf8

      parseJSONScriptMap :: Aeson.Value -> Aeson.Parser Script
      parseJSONScriptMap (Aeson.Object v) =
        v Aeson..: "cborHex" >>= parseAndDeserialize
      parseJSONScriptMap invalid =
        Aeson.prependFailure
          "parsing Script from ScriptExport failed, "
          (Aeson.typeMismatch "Object" invalid)
  parseJSON invalid =
    Aeson.prependFailure
      "parsing ScriptExport failed, "
      (Aeson.typeMismatch "Object" invalid)

{- | Bundle containing a 'Validator' and its hash.

     @since 1.1.0
-}
data ScriptInfo = ScriptInfo
  { cborHex :: Text
  -- ^ The validator script encoded as cbor hex.
  , rawHex :: Text
  -- ^ The validator script encoded as raw hex.
  }
  deriving stock
    ( -- | @since 1.1.0
      Show
    , -- | @since 1.1.0
      Eq
    , -- | @since 1.1.0
      GHC.Generic
    )
  deriving anyclass
    ( -- | @since 1.1.0
      Aeson.ToJSON
    , -- | @since 1.1.0
      Aeson.FromJSON
    )

mkScriptInfo :: Script -> ScriptInfo
mkScriptInfo script =
  let scriptRaw = LBS.toStrict $ Codec.serialise script
      scriptCBOR = CBOR.serialize' $ SBS.toShort scriptRaw
   in ScriptInfo
        { cborHex = Base16.encodeBase16 scriptCBOR
        , rawHex = Base16.encodeBase16 scriptRaw
        }

exportConfig :: Config
exportConfig =
  Config
    { tracingMode = NoTracing
    }

{- | Create a 'ScriptInfo' given a Plutarch term of a policy.

     @since 1.1.0
-}
mkPolicyInfo :: ClosedTerm PMintingPolicy -> ScriptInfo
mkPolicyInfo term =
  mkScriptInfo (getMintingPolicy $ mkMintingPolicy exportConfig term)

{- | Create a 'ScriptInfo' given a Plutarch term of a validator.

     @since 1.1.0
-}
mkValidatorInfo :: ClosedTerm PValidator -> ScriptInfo
mkValidatorInfo term =
  mkScriptInfo (getValidator $ mkValidator exportConfig term)

{- | Create a 'ScriptInfo' given a Plutarch term of a stake validator.

     @since 1.1.1
-}
mkStakeValidatorInfo :: ClosedTerm PStakeValidator -> ScriptInfo
mkStakeValidatorInfo term =
  mkScriptInfo (getStakeValidator $ mkStakeValidator exportConfig term)

----------------------------------------
-- Field Labels

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''ScriptExport

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''RawScriptExport
