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
  LinkerError (..),
  RoledScript (..),

  -- * Linker utilities
  runLinker,
  fetchTS,
  getParam,
  getRawScripts,
  toRoledScript,

  -- * Introduction functions
  mkScriptInfo,
  mkValidatorInfo,
  mkPolicyInfo,
  mkStakeValidatorInfo,
  mkTwoArgumentScriptInfo,
  mkThreeArgumentScriptInfo,
  toScript,

  -- * Re-exports
  ScriptRole (..),
) where

import Cardano.Binary qualified as CBOR
import Control.Monad.Except (Except, MonadError, runExcept, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Base16.Types qualified as Base16
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Map (Map, (!?))
import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Generics qualified as GHC
import Optics (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch (
  ClosedTerm,
  Config (Tracing),
  LogLevel (LogInfo),
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.LedgerApi.V2 (
  PScriptContext,  
 )
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.Orphans ()
import Plutarch.Prelude (
  PData,
  POpaque,
  (:-->),
 )
import Plutarch.Script (Script (Script), deserialiseScript, serialiseScript)
import Ply (
  ScriptRole (ThreeArgumentScript, TwoArgumentScript),
  TypedScript (TypedScript),
  TypedScriptEnvelope,
 )
import Ply.Core.TypedReader (TypedReader, mkTypedScript)
import Unsafe.Coerce (unsafeCoerce)

{- | Type for holding parameterized scripts. This type represents the
     raw scripts which needs to be linked in order to be deployed.

 @since 2.0.0
-}
newtype RawScriptExport = RawScriptExport
  { rawScripts :: Map Text TypedScriptEnvelope
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

{- | Type for holding scripts, with role information.

 @since 2.1.0
-}
data RoledScript = RoledScript
  { script :: Script
  , role :: ScriptRole
  }
  deriving stock
    ( -- | @since 2.1.0
      Show
    , -- | @since 2.1.0
      Eq
    , -- | @since 2.1.0
      GHC.Generic
    )

{- | Type for holding ready-to-go scripts. Ready-to-go scripts are
     scripts that can be used on-onchain without any extra
     arguments. They are `MintingPolicy ~ StakingValidator ~ PData ->
     PScriptContext -> POpaque` and `Validator ~ PData -> PData ->
     PScriptContext -> POpaque`.

 @since 2.0.0
-}
data ScriptExport a = ScriptExport
  { scripts :: Map Text RoledScript
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

-- | @since 2.0.0
fetchTS ::
  forall (rl :: ScriptRole) (params :: [Type]) (lparam :: Type).
  (TypedReader rl params) =>
  Text ->
  Linker lparam (TypedScript rl params)
fetchTS t = do
  (scr, _) <- ask
  case view #rawScripts scr !? t of
    Just scr' ->
      case mkTypedScript @rl @params scr' of
        Right x -> return x
        Left err -> throwError $ ScriptTypeMismatch $ pack $ show err
    Nothing -> throwError $ ScriptNotFound t

{- | Convert a TypedScript to Script.

     @since 2.1.0
-}
toScript :: forall r x. TypedScript r x -> Script
toScript s = let TypedScript _ a = unsafeCoerce s in coerce a

-- | @since 2.0.0
getParam :: forall (lparam :: Type). Linker lparam lparam
getParam = asks snd

-- | @since 2.0.0
getRawScripts :: forall (lparam :: Type). Linker lparam RawScriptExport
getRawScripts = asks fst

class ToRoledScript (rl :: ScriptRole) where
  toRoledScript' ::
    forall (params :: [Type]). TypedScript rl params -> RoledScript

instance ToRoledScript 'TwoArgumentScript where
  toRoledScript' s = RoledScript (toScript s) TwoArgumentScript

instance ToRoledScript 'ThreeArgumentScript where
  toRoledScript' s = RoledScript (toScript s) ThreeArgumentScript

{- | Convert any 'TypedScript' into 'RoledScript'.

 @since 2.1.0
-}
toRoledScript ::
  forall (rl :: ScriptRole) (param :: [Type]).
  (ToRoledScript rl) =>
  TypedScript rl param ->
  RoledScript
toRoledScript = toRoledScript'

-- Internal helper for CBOR deserialization
newtype CBORSerializedScript = CBORSerializedScript SBS.ShortByteString

instance CBOR.FromCBOR CBORSerializedScript where
  fromCBOR = CBORSerializedScript . SBS.toShort <$> CBOR.fromCBOR

cborToScript :: BS.ByteString -> Either CBOR.DecoderError Script
cborToScript x =
  deserialiseScript
    . (\(CBORSerializedScript x) -> x)
    <$> CBOR.decodeFull' x

-- | @since 2.1.0
instance Aeson.ToJSON RoledScript where
  toJSON s =
    let scr = view #script s
        raw = SBS.fromShort $ serialiseScript scr
        cbor = CBOR.serialize' $ SBS.toShort raw
     in Aeson.toJSON $
          Aeson.object
            [ "cborHex" Aeson..= (Base16.extractBase16 . Base16.encodeBase16 $ cbor)
            , "rawHex" Aeson..= (Base16.extractBase16 . Base16.encodeBase16 $ raw)
            , "hash" Aeson..= scriptHash scr
            , "role" Aeson..= view #role s
            ]

-- | @since 2.1.0
instance Aeson.FromJSON RoledScript where
  parseJSON (Aeson.Object v) =
    RoledScript
      <$> (v Aeson..: "cborHex" >>= parseAndDeserialize)
      <*> v Aeson..: "role"
    where
      parseAndDeserialize v =
        Aeson.parseJSON v
          >>= (either (fail . show) pure . cborToScript)
            . Base16.decodeBase16'
            . Base16.assertBase16
  parseJSON invalid =
    Aeson.prependFailure
      "parsing RoledScript failed, "
      (Aeson.typeMismatch "Object" invalid)

-- | @since 2.0.0
instance
  forall (a :: Type).
  (Aeson.ToJSON a) =>
  Aeson.ToJSON (ScriptExport a)
  where
  toJSON (ScriptExport scripts info) =
    Aeson.toJSON $
      Aeson.object
        [ "info" Aeson..= info
        , "scripts" Aeson..= scripts
        ]

-- | @since 2.0.0
instance
  forall (a :: Type).
  (Aeson.FromJSON a) =>
  Aeson.FromJSON (ScriptExport a)
  where
  parseJSON (Aeson.Object v) =
    ScriptExport
      <$> v Aeson..: "scripts"
      <*> v Aeson..: "info"
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
  let scriptRaw = serialiseScript script
      scriptCBOR = CBOR.serialize' scriptRaw
   in ScriptInfo
        { cborHex = Base16.extractBase16 . Base16.encodeBase16 $ scriptCBOR
        , rawHex = Base16.extractBase16 . Base16.encodeBase16 $ SBS.fromShort scriptRaw
        }

exportConfig :: Config
exportConfig = Tracing LogInfo DetTracing

{- | Create a 'ScriptInfo' given a Plutarch term of a two argument script.

     @since 2.4.0
-}
mkTwoArgumentScriptInfo :: ClosedTerm (PData :--> PScriptContext :--> POpaque) -> ScriptInfo
mkTwoArgumentScriptInfo term =
  mkScriptInfo (either (error . T.unpack) id $ compile exportConfig term)

{- | Create a 'ScriptInfo' given a Plutarch term of a three argument script.

     @since 2.4.0
-}
mkThreeArgumentScriptInfo :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque) -> ScriptInfo
mkThreeArgumentScriptInfo term =
  mkScriptInfo (either (error . T.unpack) id $ compile exportConfig term)

{- | Create a 'ScriptInfo' given a Plutarch term of a policy.

     @since 1.1.0
-}
{-# DEPRECATED mkPolicyInfo "Use 'mkTwoArgumentScriptInfo' instead." #-}
mkPolicyInfo :: ClosedTerm (PData :--> PScriptContext :--> POpaque) -> ScriptInfo
mkPolicyInfo term =
  mkScriptInfo (either (error . T.unpack) id $ compile exportConfig term)

{- | Create a 'ScriptInfo' given a Plutarch term of a validator.

     @since 1.1.0
-}
{-# DEPRECATED mkValidatorInfo "Use 'mkThreeArgumentScriptInfo' instead." #-}
mkValidatorInfo :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque) -> ScriptInfo
mkValidatorInfo term =
  mkScriptInfo (either (error . T.unpack) id $ compile exportConfig term)

{- | Create a 'ScriptInfo' given a Plutarch term of a stake validator.

     @since 1.1.1
-}
{-# DEPRECATED mkStakeValidatorInfo "Use 'mkTwoArgumentScriptInfo' instead." #-}
mkStakeValidatorInfo :: ClosedTerm (PData :--> PScriptContext :--> POpaque) -> ScriptInfo
mkStakeValidatorInfo term =
  mkScriptInfo (either (error . T.unpack) id $ compile exportConfig term)

----------------------------------------
-- Field Labels

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''ScriptExport

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''RawScriptExport

-- | @since 2.1.0
makeFieldLabelsNoPrefix ''RoledScript
