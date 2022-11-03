{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Test.Export (
  -- * Types
  ScriptParams (..),
  TestParams (..),
  ScriptOutcome (..),
  WithExport,

  -- * Functions

  -- ** Assertions
  assertValidator,
  assertMintingPolicy,

  -- ** Environment change

  -- *** Datums
  modifyingDatum,
  settingDatum,
  modifyingDatum',
  settingDatum',

  -- *** Redeemers
  modifyingRedeemer,
  settingRedeemer,
  modifyingRedeemer',
  settingRedeemer',

  -- *** Script contexts
  modifyingSC,
  settingSC,
  modifyingSC',
  settingSC',

  -- *** Multiple components
  modifyingScriptParams,
  settingScriptParams,
  modifyingScriptParams',
  settingScriptParams',

  -- ** Elimination
  exportTests,
) where

import Acc (Acc)
import Control.Monad.Trans.RWS.CPS (
  RWS,
  asks,
  execRWS,
  local,
  tell,
 )
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text, unpack)
import GHC.Exts (toList)
import Optics.At (at')
import Optics.Getter (to, view)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Lens (A_Lens, lens)
import Optics.Optic ((%), (%%))
import Optics.Setter (over, set)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Evaluate (evalScriptHuge)
import PlutusLedgerApi.V1.Scripts (applyArguments)
import PlutusLedgerApi.V2 (
  Data,
  Script,
  ScriptContext,
  ToData,
  toData,
 )
import Ply (
  ScriptRole (MintingPolicyRole, ValidatorRole),
  TypedScriptEnvelope (tsRole, tsScript),
 )
import ScriptExport.ScriptInfo (
  Linker,
  LinkerError,
  RawScriptExport,
  ScriptExport,
  runLinker,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )

{- | Typed parameters that are given to a script when testing. For minting
 policies, the redeemer is ignored; you can set it to '()' to keep the
 compiler happy.

 @since 1.2.1
-}
data ScriptParams (dat :: Type) (red :: Type) = ScriptParams
  { datum :: dat
  , redeemer :: red
  , context :: ScriptContext
  }

makeFieldLabelsNoPrefix ''ScriptParams

{- | A combination of (initial) script parameters, as well as any required
 parameters that should be passed to the linker when assembling tests.

 @since 1.2.1
-}
data TestParams (dat :: Type) (red :: Type) (p :: Type) = TestParams
  { scriptParams :: ScriptParams dat red
  , linkerParams :: p
  }

makeFieldLabelsNoPrefix ''TestParams

{- | What you expect to happen when a script is tested with specified
 parameters.

 @since 1.2.1
-}
data ScriptOutcome = Crashes | Runs
  deriving stock
    ( -- | @since 1.2.1
      Eq
    , -- | @since 1.2.1
      Ord
    , -- | @since 1.2.1
      Show
    )

{- | A helper newtype for passing script arguments implicitly, as well as
 modifying them using local scoping.

 The intended use is with @do@-notation, using a combination of contextual
 argument modifiers and assertions, then \'running\' with 'exportTests':

 > tests :: TestTree
 > tests = exportTests "my tests" export linker testParams $ do
 >    assertValidator Crashes "my context is bad" "script1"
 >    modifyingSC fixUpContext $ do
 >      assertValidator Crashes "context fine, redeemer bad" "script1"
 >      modifyingRedeemer fixUpRedeemer $ do
 >        assertValidator Runs "now everything is fine" "script1"
 >      -- Redeemer fix-up resets here
 >      assertMintingPolicy Crashes "inappropriate datum" "script2"
 >      settingDatum' redeemerDatum $ do
 >        assertMintingPolicy Runs "appropriate datum" "script2"

 You can also define separate \'bits\' of a suite using 'WithExport' directly.

 @since 1.2.1
-}
newtype WithExport (dat :: Type) (red :: Type) (info :: Type) (a :: Type)
  = WithExport (RWS (WithExportEnv dat red info) WithExportLog () a)
  deriving
    ( -- | @since 1.2.1
      Functor
    , -- | @since 1.2.1
      Applicative
    , -- | @since 1.2.1
      Monad
    )
    via ( RWS (WithExportEnv dat red info) WithExportLog ()
        )

{- | Given a name for the overall test suite, a raw export, a linker, and some
 test parameters, assemble a 'TestTree'.

 This acts as a 'testGroup' replacement, and can be used in a similar way. The
 exact number of tests this will generate depends on the script export given,
 and the scripts we ask to test:

 - If linking fails, you will get a single, always-failing, test, which
 presents the linker error;
 - If any tests request non-existent scripts (that is, a name that doesn't
 correspond to an existing script), those requests will be bundled into a
 single, always-failing, test, which presents the name that doesn't have a
 script associated with it;
 - Otherwise, one test per request.

 @since 1.2.1
-}
exportTests ::
  forall (dat :: Type) (red :: Type) (info :: Type) (param :: Type).
  String ->
  RawScriptExport ->
  Linker param (ScriptExport info) ->
  TestParams dat red param ->
  WithExport dat red info () ->
  TestTree
exportTests name raw linker args comp = case runWithExport comp raw linker args of
  Left err -> singleTest "Linking" . FailedToLink $ err
  Right result -> testGroup name . toList . Map.foldMapWithKey go $ result
    where
      go :: Text -> PreparedScriptTests -> Acc TestTree
      go testName = \case
        NoSuchScript -> pure . singleTest "Presence" . MissingScript $ testName
        PreparedTests tests -> fmap prepDataToTest tests

{- | States that the named validator should have the given outcome, using the
 currently in-scope arguments. The test will be labelled with the given
 explanation (or reason) in the suite, as long as the named script actually
 exists.

 = Example

 > tests = do
 >    assertValidator Crashes "reason for crash" "myScript"
 >    assertValidator Runs "reason for success" "aBetterScript"

 @since 1.2.1
-}
assertValidator ::
  forall (dat :: Type) (red :: Type) (info :: Type).
  (ToData dat, ToData red) =>
  -- | What you expect to happen
  ScriptOutcome ->
  -- | The reason or explanation to label the test with
  String ->
  -- | The name of the script to run
  Text ->
  WithExport dat red info ()
assertValidator outcome reason name = WithExport $ do
  lookedUp <- asks (view (#raws % #rawScripts % at' @(Map _ _) name))
  case lookedUp of
    Nothing -> tell . noSuchScript $ name
    Just envelope -> case view (to tsRole) envelope of
      ValidatorRole -> do
        datum' <- asks (toData . view #datum)
        redeemer' <- asks (toData . view #redeemer)
        sc' <- asks (toData . view #context)
        let script = view (to tsScript) envelope
        let result = AppliedValidator script datum' redeemer' sc'
        tell . prepareTest name reason outcome $ result
      MintingPolicyRole -> tell . roleMismatch name reason $ ValidatorRole

{- | As 'assertValidator', but for a minting policy instead.

 @since 1.2.1
-}
assertMintingPolicy ::
  forall (dat :: Type) (red :: Type) (info :: Type).
  (ToData dat) =>
  -- | What you expect to happen
  ScriptOutcome ->
  -- | The reason or explanation to label the test with
  String ->
  -- | The name of the script to run
  Text ->
  WithExport dat red info ()
assertMintingPolicy outcome reason name = WithExport $ do
  lookedUp <- asks (view (#raws % #rawScripts % at' @(Map _ _) name))
  case lookedUp of
    Nothing -> tell . noSuchScript $ name
    Just envelope -> case tsRole envelope of
      ValidatorRole -> tell . roleMismatch name reason $ MintingPolicyRole
      MintingPolicyRole -> do
        datum' <- asks (toData . view #datum)
        sc' <- asks (toData . view #context)
        let script = tsScript envelope
        let result = AppliedMP script datum' sc'
        tell . prepareTest name reason outcome $ result

{- | Modify the given test collection to use a different datum, based on the
 provided function. The additional information provided by the script export
 is provided as an additional parameter to the function argument.

 = Example

 > tests = do
 >    -- This will use unmodified arguments
 >    assertValidator Crashes "bad datum" "myScript"
 >    modifyingDatum fixMyDatum $ do
 >      -- This will use a datum modified according to fixMyDatum
 >      assertValidator Runs "good datum" "myScript"
 >      -- So will this
 >      assertValidator Crashes "this one is wrong" "myScript2"
 >    -- However, here, the change resets
 >    assertValidator Runs "but this one is good" "myScript2"

 You can \'nest\' modifications, which behave according to lexical scope:

 > tests = do
 >    -- One change
 >    modifyingDatum change1 $ do
 >      -- This will only modify with change1
 >      assertValidator Crashes "not enough fixes" "myScript"
 >      -- Stack on another change
 >      modifyingDatum change2 $ do
 >        -- This modifies with change1, then change2 on top
 >        assertValidator Runs "enough fixes" "myScript"
 >      -- reset change2
 >      assertValidator Crashes "why did we undo this change?" "myScript"
 >   -- reset change1

 @since 1.2.1
-}
modifyingDatum ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the datum
  (info -> dat -> dat) ->
  -- | The collection of tests to run using the modified datum as an argument
  WithExport dat red info a ->
  WithExport dat red info a
modifyingDatum f (WithExport comp) =
  WithExport . local (\env -> over #datum (f (view #info env)) env) $ comp

{- | As 'modifyingDatum', except the \'locally scoped\' datum is replaced with
 the given value.

 @since 1.2.1
-}
settingDatum ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally set\' the datum
  (info -> dat) ->
  -- | The collection of tests to run using the set datum as an argument
  WithExport dat red info a ->
  WithExport dat red info a
settingDatum f = modifyingDatum (\i _ -> f i)

{- | As 'modifyingDatum', but without additional information from the script
 export.

 @since 1.2.1
-}
modifyingDatum' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the datum
  (dat -> dat) ->
  -- | The collection of tests to run using the modified datum as an argument
  WithExport dat red info a ->
  WithExport dat red info a
modifyingDatum' f = modifyingDatum (\_ x -> f x)

{- | As 'settingDatum', but without additional information from the script
 export.

 @since 1.2.1
-}
settingDatum' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | A \'local value\' for the datum
  dat ->
  -- | The collection of tests to run using the set datum as an argument
  WithExport dat red info a ->
  WithExport dat red info a
settingDatum' x = modifyingDatum (\_ _ -> x)

{- | Modify the given test collection to use a different redeemer, based on the
 provided function. The additional information provided by the script export
 is provided as an additional parameter to the function argument.

 Use this similarly to 'modifyingDatum': see its documentation for an example of use.

 @since 1.2.1
-}
modifyingRedeemer ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the redeemer
  (info -> red -> red) ->
  -- | The collection of tests to run using the modified redeemer as an argument
  WithExport dat red info a ->
  WithExport dat red info a
modifyingRedeemer f (WithExport comp) =
  WithExport . local (\env -> over #redeemer (f (view #info env)) env) $ comp

{- | As 'modifyingRedeemer', except the \'locally scoped\' redeemer is replaced with
 the given value.

 @since 1.2.1
-}
settingRedeemer ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally set\' the redeemer
  (info -> red) ->
  -- | The collection of tests to run using the set redeemer as an argument
  WithExport dat red info a ->
  WithExport dat red info a
settingRedeemer f = modifyingRedeemer (\i _ -> f i)

{- | As 'modifyingRedeemer', but without additional information from the script
 export.

 @since 1.2.1
-}
modifyingRedeemer' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the redeemer
  (red -> red) ->
  -- | The collection of tests to run using the modified redeemer as an argument
  WithExport dat red info a ->
  WithExport dat red info a
modifyingRedeemer' f = modifyingRedeemer (\_ x -> f x)

{- | As 'settingRedeemer', but without additional information from the script
 export.

 @since 1.2.1
-}
settingRedeemer' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | A \'local value\' for the redeemer
  red ->
  -- | The collection of tests to run using the set redeemer as an argument
  WithExport dat red info a ->
  WithExport dat red info a
settingRedeemer' x = modifyingRedeemer (\_ _ -> x)

{- | Modify the given test collection to use a different context, based on the
 provided function. The additional information provided by the script export
 is provided as an additional parameter to the function argument.

 Use this similarly to 'modifyingDatum': see its documentation for an example of use.

 @since 1.2.1
-}
modifyingSC ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the context
  (info -> ScriptContext -> ScriptContext) ->
  -- | The collection of tests to run using the modified context as an argument
  WithExport dat red info a ->
  WithExport dat red info a
modifyingSC f (WithExport comp) =
  WithExport . local (\env -> over #context (f (view #info env)) env) $ comp

{- | As 'modifyingSC', except the \'locally scoped\' context is replaced with
 the given value.

 @since 1.2.1
-}
settingSC ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally set\' the context
  (info -> ScriptContext) ->
  -- | The collection of tests to run using the set context as an argument
  WithExport dat red info a ->
  WithExport dat red info a
settingSC f = modifyingSC (\i _ -> f i)

{- | As 'modifyingSC', but without additional information from the script
 export.

 @since 1.2.1
-}
modifyingSC' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the context
  (ScriptContext -> ScriptContext) ->
  -- | The collection of tests to run using the modified context as an argument
  WithExport dat red info a ->
  WithExport dat red info a
modifyingSC' f = modifyingSC (\_ x -> f x)

{- | As 'settingSC', but without additional information from the script
 export.

 @since 1.2.1
-}
settingSC' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | A \'local value\' for the context
  ScriptContext ->
  -- | The collection of tests to run using the set context as an argument
  WithExport dat red info a ->
  WithExport dat red info a
settingSC' x = modifyingSC (\_ _ -> x)

{- | Modify the given test collection to use different script arguments, based
 on the provided function. This is equivalent to nesting the other environment
 changing functions, but can be more convenient. The additional information
 provided by the script export is provided as an additional parameter to the
 function argument.

 Use this similarly to 'modifyingDatum': see its documentation for an example of use.

 @since 1.2.1
-}
modifyingScriptParams ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the script arguments
  (info -> ScriptParams dat red -> ScriptParams dat red) ->
  -- | The collection of tests to run using the modified arguments
  WithExport dat red info a ->
  WithExport dat red info a
modifyingScriptParams f (WithExport comp) = WithExport . local go $ comp
  where
    go :: WithExportEnv dat red info -> WithExportEnv dat red info
    go env =
      let collected =
            ScriptParams
              (view #datum env)
              (view #redeemer env)
              (view #context env)
          modified = f (view #info env) collected
       in set #datum (view #datum modified)
            . set #redeemer (view #redeemer modified)
            . set #context (view #context modified)
            $ env

{- | As 'modifyingScriptParams', except the \'locally scoped\' arguments are replaced with
 the given ones.

 @since 1.2.1
-}
settingScriptParams ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally set\' the script arguments
  (info -> ScriptParams dat red) ->
  -- | The collection of tests to run using the set arguments
  WithExport dat red info a ->
  WithExport dat red info a
settingScriptParams f = modifyingScriptParams (\i _ -> f i)

{- | As 'modifyingScriptParams', but without additional information from the script
 export.

 @since 1.2.1
-}
modifyingScriptParams' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | How to \'locally modify\' the arguments
  (ScriptParams dat red -> ScriptParams dat red) ->
  -- | The collection of tests to run using the modified arguments
  WithExport dat red info a ->
  WithExport dat red info a
modifyingScriptParams' f = modifyingScriptParams (\_ x -> f x)

{- | As 'settingScriptParams', but without additional information from the script
 export.

 @since 1.2.1
-}
settingScriptParams' ::
  forall (dat :: Type) (red :: Type) (info :: Type) (a :: Type).
  -- | A \'local value\' for the arguments
  ScriptParams dat red ->
  -- | The collection of tests to run using the set arguments
  WithExport dat red info a ->
  WithExport dat red info a
settingScriptParams' x = modifyingScriptParams (\_ _ -> x)

-- Helpers

runWithExport ::
  forall (dat :: Type) (red :: Type) (info :: Type) (param :: Type).
  WithExport dat red info () ->
  RawScriptExport ->
  Linker param (ScriptExport info) ->
  TestParams dat red param ->
  Either LinkerError (Map Text PreparedScriptTests)
runWithExport (WithExport comp) raw linker args = do
  linked <- runLinker linker raw . view #linkerParams $ args
  let env =
        WithExportEnv
          (view (#scriptParams %% #datum) args)
          (view (#scriptParams %% #redeemer) args)
          (view (#scriptParams %% #context) args)
          raw
          (view #information linked)
  pure . coerce . snd . execRWS comp env $ ()

-- Note from Koz: the manual lenses are due to a weird interaction between TH
-- and record syntax. I wasn't able to establish _why_ it was being weird, but
-- when written this way, it behaves.
data WithExportEnv (dat :: Type) (red :: Type) (info :: Type)
  = WithExportEnv dat red ScriptContext RawScriptExport info

instance
  (k ~ A_Lens, a ~ dat, b ~ dat) =>
  LabelOptic "datum" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b
  where
  labelOptic = lens (\(WithExportEnv x _ _ _ _) -> x) $ \(WithExportEnv _ r sc ra i) d' ->
    WithExportEnv d' r sc ra i

instance
  (k ~ A_Lens, a ~ red, b ~ red) =>
  LabelOptic "redeemer" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b
  where
  labelOptic = lens (\(WithExportEnv _ x _ _ _) -> x) $ \(WithExportEnv d _ sc ra i) r' ->
    WithExportEnv d r' sc ra i

instance
  (k ~ A_Lens, a ~ ScriptContext, b ~ ScriptContext) =>
  LabelOptic "context" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b
  where
  labelOptic = lens (\(WithExportEnv _ _ x _ _) -> x) $ \(WithExportEnv d r _ ra i) sc' ->
    WithExportEnv d r sc' ra i

instance
  (k ~ A_Lens, a ~ RawScriptExport, b ~ RawScriptExport) =>
  LabelOptic "raws" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b
  where
  labelOptic = lens (\(WithExportEnv _ _ _ x _) -> x) $ \(WithExportEnv d r sc _ i) raws' ->
    WithExportEnv d r sc raws' i

instance
  (k ~ A_Lens, a ~ info, b ~ info) =>
  LabelOptic "info" k (WithExportEnv dat red info) (WithExportEnv dat red info) a b
  where
  labelOptic = lens (\(WithExportEnv _ _ _ _ x) -> x) $ \(WithExportEnv d r sc ra _) i' ->
    WithExportEnv d r sc ra i'

data Applied
  = AppliedValidator Script Data Data Data
  | AppliedMP Script Data Data

data PreparedTestData
  = WrongRole String ScriptRole
  | PreparedData String ScriptOutcome Applied

data PreparedScriptTests
  = NoSuchScript
  | PreparedTests (Acc PreparedTestData)

instance Semigroup PreparedScriptTests where
  {-# INLINEABLE (<>) #-}
  NoSuchScript <> _ = NoSuchScript
  _ <> NoSuchScript = NoSuchScript
  PreparedTests acc <> PreparedTests acc' = PreparedTests $ acc <> acc'

newtype WithExportLog = WithExportLog (Map Text PreparedScriptTests)

instance Semigroup WithExportLog where
  {-# INLINEABLE (<>) #-}
  WithExportLog m1 <> WithExportLog m2 =
    WithExportLog . Map.unionWith (<>) m1 $ m2

instance Monoid WithExportLog where
  {-# INLINEABLE mempty #-}
  mempty = WithExportLog Map.empty

noSuchScript :: Text -> WithExportLog
noSuchScript scriptName =
  WithExportLog . Map.singleton scriptName $ NoSuchScript

prepareTest ::
  Text ->
  String ->
  ScriptOutcome ->
  Applied ->
  WithExportLog
prepareTest name reason outcome =
  WithExportLog
    . Map.singleton name
    . PreparedTests
    . pure
    . PreparedData reason outcome

roleMismatch :: Text -> String -> ScriptRole -> WithExportLog
roleMismatch name reason =
  WithExportLog
    . Map.singleton name
    . PreparedTests
    . pure
    . WrongRole reason

newtype FailedToLink = FailedToLink LinkerError

instance IsTest FailedToLink where
  run _ (FailedToLink err) _ =
    pure . testFailed $
      "Could not link:\n"
        <> show err
  testOptions = Tagged []

newtype MissingScript = MissingScript Text

instance IsTest MissingScript where
  run _ (MissingScript name) _ =
    pure . testFailed $
      "No such script: " <> unpack name
  testOptions = Tagged []

newtype RoleMisAssigned = RoleMisAssigned ScriptRole

instance IsTest RoleMisAssigned where
  run _ (RoleMisAssigned intended) _ =
    pure . testFailed $ case intended of
      ValidatorRole ->
        "Declared as minting policy, but script is a validator."
      MintingPolicyRole ->
        "Declared as a validator, but script is a minting policy."
  testOptions = Tagged []

prepDataToTest :: PreparedTestData -> TestTree
prepDataToTest = \case
  WrongRole reason intended ->
    singleTest reason . RoleMisAssigned $ intended
  PreparedData reason outcome applied ->
    singleTest reason . RunWithExpectation outcome $ applied

data RunWithExpectation = RunWithExpectation ScriptOutcome Applied

instance IsTest RunWithExpectation where
  run _ (RunWithExpectation outcome applied) _ = do
    let withArgs = case applied of
          AppliedValidator script datum redeemer sc ->
            applyArguments script [datum, redeemer, sc]
          AppliedMP script datum sc ->
            applyArguments script [datum, sc]
    let (res, _, logs) = evalScriptHuge withArgs
    case (res, outcome) of
      (Left _, Crashes) -> pure . testPassed $ "Crashed as expected"
      (Left err, Runs) ->
        pure . testFailed $
          "Expected to run, but crashed instead.\n"
            <> "Error: "
            <> show err
            <> "\n"
            <> "Logs:\n\n"
            <> show logs
      (Right _, Crashes) ->
        pure . testFailed $
          "Expected to crash, but ran instead.\n"
            <> "Logs:\n\n"
            <> show logs
      (Right _, Runs) -> pure . testPassed $ "Ran as expected"
  testOptions = Tagged []
