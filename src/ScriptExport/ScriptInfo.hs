{- |
Module     : Agora.ScriptInfo
Maintainer : emi@haskell.fyi
Description: Exportable script bundles for off-chain consumption.

Exportable script bundles for off-chain consumption.
-}
module ScriptExport.ScriptInfo (
  -- * Types
  ScriptInfo (..),

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
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Plutarch (ClosedTerm, Config (Config, tracingMode), TracingMode (NoTracing))
import Plutarch.Api.V2 (PMintingPolicy, PStakeValidator, PValidator, mkMintingPolicy, mkStakeValidator, mkValidator, scriptHash)
import PlutusLedgerApi.V2 (MintingPolicy (getMintingPolicy), Script, ScriptHash, StakeValidator (getStakeValidator), Validator (getValidator))

{- | Bundle containing a 'Validator' and its hash.

     @since 1.1.0
-}
data ScriptInfo = ScriptInfo
  { cborHex :: Text
  -- ^ The validator script encoded as cbor hex.
  , rawHex :: Text
  -- ^ The validator script encoded as raw hex.
  , hash :: ScriptHash
  -- ^ Hash of the validator.
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
        , hash = scriptHash script
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
