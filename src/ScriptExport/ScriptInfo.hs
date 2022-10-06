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

  -- * Introduction functions
  mkScriptInfo,
  mkValidatorInfo,
  mkPolicyInfo,
  mkStakeValidatorInfo,
) where

import Aeson.Orphans ()
import Cardano.Binary qualified as CBOR
import Codec.Serialise qualified as Codec
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Text (Text, pack)
import GHC.Generics qualified as GHC
import Optics (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch (ClosedTerm, Config (Config, tracingMode), TracingMode (NoTracing))
import Plutarch.Api.V2 (PMintingPolicy, PStakeValidator, PValidator, mkMintingPolicy, mkStakeValidator, mkValidator)
import PlutusLedgerApi.V2 (MintingPolicy (getMintingPolicy), Script, StakeValidator (getStakeValidator), Validator (getValidator))

import Control.Monad.Except
import Control.Monad.Reader
import Data.Kind (Type)
import Data.Map (Map, (!?))
import Ply (ScriptRole, TypedScript, TypedScriptEnvelope)
import Ply.Core.TypedReader (TypedReader, mkTypedScript)

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
     raw scriptst which needs to be linked in order to be deployed.

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

{- | Type for holding ready-to-go scripts.

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
  deriving anyclass
    ( -- | @since 2.0.0
      Aeson.ToJSON
    , -- | @since 2.0.0
      Aeson.FromJSON
    )

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
