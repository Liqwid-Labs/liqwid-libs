{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module Plutarch.Test.Export (
  -- * Types
  ScriptParams (..),
  TestParams (..),
  ScriptOutcome (..),
  WithExport,
  WithExportError (..),
  -- * Functions
  -- ** Assertions
  assertValidator,
  assertMintingPolicy,
  -- ** Environment change
  modifyingDatum,
  modifyingRedeemer,
  modifyingSC,
  -- ** Elimination
  exportTests,
  ) where

import Test.Tasty.Providers (singleTest)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Semigroup (stimes)
import Test.Tasty (TestTree, testGroup)
import GHC.Exts (toList)
import Optics.Setter (over)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Lens (A_Lens, lens)
import Data.Map (Map)
import Ply (ScriptRole (ValidatorRole, MintingPolicyRole), 
  TypedScriptEnvelope (tsRole, tsScript))
import Optics.At (at')
import Optics.Optic ((%), (%%))
import Acc (Acc)
import ScriptExport.ScriptInfo (RawScriptExport, Linker, ScriptExport,
  runLinker, LinkerError)
import Data.Text (Text)
import Optics.Getter (view, to)
import Data.Kind (Type)
import PlutusLedgerApi.V2 (ScriptContext, Script, Data, toData, 
  ToData)
import Optics.TH (makeFieldLabelsNoPrefix)
import Control.Monad.Trans.RWS.CPS (RWS, tell, asks, local, 
  execRWS)

-- | @since 1.2.0
data ScriptParams (dat :: Type) (red :: Type) = 
  ScriptParams {
    datum :: dat,
    redeemer :: red,
    context :: ScriptContext
    }

makeFieldLabelsNoPrefix ''ScriptParams

-- | @since 1.2.0
data TestParams (dat :: Type) (red :: Type) (p :: Type) = 
  TestParams {
    scriptParams :: ScriptParams dat red,
    linkerParams :: p
    }

makeFieldLabelsNoPrefix ''TestParams

-- | @since 1.2.0
data ScriptOutcome = Crashes | Runs
  deriving stock (
    -- | @since 1.2.0
    Eq, 
    -- | @since 1.2.0
    Ord, 
    -- | @since 1.2.0
    Show
    )

-- | @since 1.2.0
data WithExportError = NoSuchScript Text | 
  RoleMismatch Text ScriptRole |
  LinkingFailed LinkerError
  deriving stock (
    -- | @since 1.2.0
    Eq,
    -- | @since 1.2.0
    Show)

-- | @since 1.2.0
newtype WithExport (dat :: Type) (red :: Type) (info :: Type) (a :: Type) = 
  WithExport (RWS (WithExportEnv dat red info) WithExportLog () a) 
  deriving (
    -- | @since 1.2.0
    Functor, 
    -- | @since 1.2.0
    Applicative) via (
      RWS (WithExportEnv dat red info) WithExportLog ()
      )

runWithExport :: forall (dat :: Type) (red :: Type) (info :: Type) (param :: Type) . 
  WithExport dat red info () -> 
  RawScriptExport -> 
  Linker param (ScriptExport info) ->
  TestParams dat red param ->  
  Either LinkerError (Map Text (Acc (Either WithExportError Applied)))
runWithExport (WithExport comp) raw linker args = 
  case runLinker linker raw . view #linkerParams $ args of 
    Left err -> Left err
    Right linked -> let env = WithExportEnv (view (#scriptParams %% #datum) args)
                                            (view (#scriptParams %% #redeemer) args) 
                                            (view (#scriptParams %% #context) args) 
                                            raw 
                                            (view #information linked) in
      Right . coerce . snd . execRWS comp env $ () 

-- | @since 1.2.0
exportTests :: forall (dat :: Type) (red :: Type) (info :: Type) (param :: Type) .
  String -> 
  RawScriptExport -> 
  Linker param (ScriptExport info) -> 
  TestParams dat red param -> 
  WithExport dat red info () -> 
  TestTree
exportTests name raw linker args comp = case runWithExport comp raw linker args of
  Left err -> singleTest "Linking" _
  Right result -> _
{-
  let (errs, applieds) = runWithExport comp raw linker args in 
    testGroup name $ fmap errToTest errs <> fmap appliedToTest applieds
-}

-- | @since 1.2.0
assertValidator :: forall (dat :: Type) (red :: Type) (info :: Type)  .
  (ToData dat, ToData red) =>
  Text -> 
  ScriptOutcome -> 
  WithExport dat red info ()
assertValidator name outcome = WithExport $ do
  lookedUp <- asks (view (#raws % #rawScripts % at' @(Map _ _) name))
  case lookedUp of
    Nothing -> _ -- tell . failed . NoSuchScript $ name
    Just envelope -> case view (to tsRole) envelope of 
      ValidatorRole -> do
        datum' <- asks (toData . view #datum)
        redeemer' <- asks (toData . view #redeemer)
        sc' <- asks (toData . view #context)
        let script = view (to tsScript) envelope
        let result = AppliedValidator outcome script datum' redeemer' sc'
        _ -- tell . succeeded $ result 
      MintingPolicyRole -> _ -- tell . failed . RoleMismatch name $ ValidatorRole

-- | @since 1.2.0
assertMintingPolicy :: forall (dat :: Type) (red :: Type) (info :: Type)  . 
  (ToData dat) =>
  Text -> 
  ScriptOutcome -> 
  WithExport dat red info ()
assertMintingPolicy name outcome = WithExport $ do
  lookedUp <- asks (view (#raws % #rawScripts % at' @(Map _ _) name))
  case lookedUp of 
    Nothing -> _ -- tell . failed . NoSuchScript $ name
    Just envelope -> case tsRole envelope of 
      ValidatorRole -> _ -- tell . failed. RoleMismatch name $ MintingPolicyRole 
      MintingPolicyRole -> do
        datum' <- asks (toData . view #datum)
        sc' <- asks (toData . view #context)
        let script = tsScript envelope
        let result = AppliedMP outcome script datum' sc'
        _ -- tell . succeeded $ result

-- | @since 1.2.0
modifyingDatum :: forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type) . 
  (info -> dat -> dat) -> 
  WithExport dat red info a -> 
  WithExport dat red info a
modifyingDatum f (WithExport comp) = 
  WithExport . local (\env -> over #datum (f (view #info env)) env) $ comp

-- | @since 1.2.0
modifyingRedeemer :: forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type) . 
  (info -> red -> red) -> 
  WithExport dat red info a -> 
  WithExport dat red info a
modifyingRedeemer f (WithExport comp) = 
  WithExport . local (\env -> over #redeemer (f (view #info env)) env) $ comp

-- | @since 1.2.0
modifyingSC :: forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type) . 
  (info -> ScriptContext -> ScriptContext) -> 
  WithExport dat red info a -> 
  WithExport dat red info a
modifyingSC f (WithExport comp) = 
  WithExport . local (\env -> over #context (f (view #info env)) env) $ comp

-- Helpers

data WithExportEnv (dat :: Type) (red :: Type) (info :: Type) = 
  WithExportEnv dat red ScriptContext RawScriptExport info

instance (k ~ A_Lens, a ~ dat, b ~ dat) => 
  LabelOptic "datum" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b where
  labelOptic = lens (\(WithExportEnv x _ _ _ _) -> x) $ \(WithExportEnv _ r sc ra i) d' -> 
    WithExportEnv d' r sc ra i

instance (k ~ A_Lens, a ~ red, b ~ red) => 
  LabelOptic "redeemer" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b where
  labelOptic = lens (\(WithExportEnv _ x _ _ _) -> x) $ \(WithExportEnv d _ sc ra i) r' -> 
    WithExportEnv d r' sc ra i

instance (k ~ A_Lens, a ~ ScriptContext, b ~ ScriptContext) => 
  LabelOptic "context" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b where
  labelOptic = lens (\(WithExportEnv _ _ x _ _) -> x) $ \(WithExportEnv d r _ ra i) sc' -> 
    WithExportEnv d r sc' ra i

instance (k ~ A_Lens, a ~ RawScriptExport, b ~ RawScriptExport) => 
  LabelOptic "raws" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b where
  labelOptic = lens (\(WithExportEnv _ _ _ x _) -> x) $ \(WithExportEnv d r sc _ i) raws' -> 
    WithExportEnv d r sc raws' i

instance (k ~ A_Lens, a ~ info, b ~ info) => 
  LabelOptic "info" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b where
  labelOptic = lens (\(WithExportEnv _ _ _ _ x) -> x) $ \(WithExportEnv d r sc ra _) i' -> 
    WithExportEnv d r sc ra i'

newtype WithExportLog = WithExportLog (Map Text (Acc (Either WithExportError Applied)))

instance Semigroup WithExportLog where
  {-# INLINEABLE (<>) #-}
  WithExportLog m1 <> WithExportLog m2 = 
    WithExportLog . Map.unionWith (<>) m1 $ m2
  {-# INLINEABLE stimes #-}
  stimes reps (WithExportLog m) = WithExportLog . fmap (stimes reps) $ m

instance Monoid WithExportLog where
  {-# INLINEABLE mempty #-}
  mempty = WithExportLog Map.empty

data Applied = 
  AppliedValidator ScriptOutcome Script Data Data Data | 
  AppliedMP ScriptOutcome Script Data Data

errToTest :: WithExportError -> TestTree
errToTest err = _

appliedToTest :: Applied -> TestTree
appliedToTest applied = _
