{-# LANGUAGE OverloadedLists #-}
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

  -- ** Scoped parameter changes

  -- *** Script context
  localScriptContext,
  localScriptContext',
  setScriptContext,
  setScriptContext',

  -- *** All parameters at once
  localScriptParams,
  localScriptParams',
  setScriptParams,
  setScriptParams',

  -- ** Elimination
  withLinked,
  withRaw,
) where

import Acc (Acc)
import Control.Monad.Trans.RWS.CPS (
  RWS,
  asks,
  execRWS,
  tell,
  withRWS,
 )
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text, unpack)
import GHC.Exts (toList)
import Optics.AffineFold (preview)
import Optics.AffineTraversal (An_AffineTraversal, atraversal)
import Optics.At (at)
import Optics.Getter (view)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Lens (A_Lens, lens)
import Optics.Optic ((%), (%%))
import Optics.Setter (over)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Evaluate (evalScriptHuge)
import Plutarch.Extra.Script (applyArguments)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (
  Data,
  ScriptContext,
  ToData,
  toData,
 )
import ScriptExport.ScriptInfo (
  Linker,
  LinkerError,
  RawScriptExport,
  ScriptExport,
  ScriptRole (MintingPolicyRole, ValidatorRole),
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
data ScriptParams (datum :: Type) (redeemer :: Type) = ScriptParams
  { datum :: datum
  , redeemer :: redeemer
  , context :: ScriptContext
  }

-- | @since 1.2.1
makeFieldLabelsNoPrefix ''ScriptParams

{- | A combination of (initial) script parameters, as well as any required
 parameters that should be passed to the linker when assembling tests.

 @since 1.2.1
-}
data TestParams (datum :: Type) (redeemer :: Type) (params :: Type) = TestParams
  { scriptParams :: ScriptParams datum redeemer
  , linkerParams :: params
  }

-- | @since 1.2.1
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

{- | A description of a batch of tests, with implicitly-passed parameters. Also
 contains some metadata from 'ScriptExport's.

 While this is a 'Monad', it's really designed only to be used with the
 operations defined in this module. We provide this API mostly for
 convenience.

 = See also

 - 'assertValidator' and 'assertMintingPolicy' for defining tests;
 - 'localScriptParams' and related functions for scoped modifications to
    implicit parameters;
 - 'withLinked' to build tests from a description using a pre-linked
   'ScriptExport';
 - 'withRaw' to pass a 'RawScriptExport' and a linker explicitly to build
    tests.

 @since 1.2.1
-}
newtype WithExport (datum :: Type) (redeemer :: Type) (info :: Type) (a :: Type)
  = WithExport (RWS (Env datum redeemer info) TestMap () a)
  deriving
    ( -- | @since 1.2.1
      Functor
    , -- | @since 1.2.1
      Applicative
    , -- | @since 1.2.1
      Monad
    )
    via (RWS (Env datum redeemer info) TestMap ())

{- | Specify that the validator under the given name should have the given
 outcome, using the currently in-scope parameters. The test will be labelled
 with the given explanation (or reason) in the suite, as long as the named
 script exists.

 = Example

 > tests = do
 >    assertValidator Crashes "reason for crash" "myScript"
 >    assertValidator Runs "reason for success" "aBetterScript"

 @since 1.2.1
-}
assertValidator ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type).
  (ToData datum, ToData redeemer) =>
  -- | What you expect to happen
  ScriptOutcome ->
  -- | The explanation (or reason) to label the test with
  String ->
  -- | Name of the script to run
  Text ->
  WithExport datum redeemer info ()
assertValidator outcome reason name = WithExport $ do
  lookedUp <- asks (view (#exports % #scripts % at name))
  case lookedUp of
    Nothing -> tell . noSuchScript $ name
    Just found -> case view #role found of
      ValidatorRole -> do
        sp <- asks (view #params)
        tell . prepareValidator name reason (view #script found) outcome $ sp
      MintingPolicyRole -> tell . wrongRole name reason $ ValidatorRole

{- | Specify that the minting policy under the given name should have the given
 outcome, using the currently in-scope parameters. The test will be labelled
 with the given explanation (or reason) in the suite, as long as the named
 script exists.

 = Example

 > tests = do
 >    assertMintingPolicy Crashes "reason for crash" "myPolicy"
 >    assertMintingPolicy Runs "reason for success" "aBetterPolicy"

 @since 1.2.1
-}
assertMintingPolicy ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type).
  (ToData datum) =>
  -- | What you expect to happen
  ScriptOutcome ->
  -- | The explanation (or reason) to label the test with
  String ->
  -- | Name of the script to run
  Text ->
  WithExport datum redeemer info ()
assertMintingPolicy outcome reason name = WithExport $ do
  lookedUp <- asks (view (#exports % #scripts % at name))
  case lookedUp of
    Nothing -> tell . noSuchScript $ name
    Just found -> case view #role found of
      ValidatorRole -> tell . wrongRole name reason $ MintingPolicyRole
      MintingPolicyRole -> do
        sp <- asks (view #params)
        tell . prepareMintingPolicy name reason (view #script found) outcome $ sp

{- | Given a function to modify the 'ScriptParams' used for test definitions,
 and a collection of tests, construct those tests using parameters modified
 according to the function. The function accepts an additional parameter,
 which is metadata from the script export.

 The \'changes\' made by the function argument are scoped /only/ to the given
 collection of tests:

 > tests = do
 >    -- Here, we use the unmodified parameters
 >    assertMintingPolicy Crashes "bad params" "aPolicy"
 >    localScriptParams fixBadParams $ do
 >      -- Here, the parameters are modified by fixBadParams
 >      assertMintingPolicy Runs "good params" "aPolicy"
 >    -- The parameters reset, as we've left the scope
 >    assertMintingPolicy Crashs "params are bad again" "aPolicy"

 = Note

 The \'modification function\' is allowed to change the types of the datum
 or redeemer (or both): in particular, you can use raw 'Data' if you wish.

 @since 1.2.1
-}
localScriptParams ::
  forall
    (datum :: Type)
    (datum' :: Type)
    (redeemer :: Type)
    (redeemer' :: Type)
    (info :: Type)
    (a :: Type).
  (info -> ScriptParams datum redeemer -> ScriptParams datum' redeemer') ->
  WithExport datum' redeemer' info a ->
  WithExport datum redeemer info a
localScriptParams f (WithExport comp) = WithExport $ do
  information <- asks (view (#exports %% #information))
  withRWS (go information) comp
  where
    go ::
      info ->
      Env datum redeemer info ->
      () ->
      (Env datum' redeemer' info, ())
    go information env () = (over #params (f information) env, ())

{- | As 'localScriptParams', but without using the metadata parameter for the
 function argument.

 @since 1.2.1
-}
localScriptParams' ::
  forall (datum :: Type) (datum' :: Type) (redeemer :: Type) (redeemer' :: Type) (info :: Type) (a :: Type).
  (ScriptParams datum redeemer -> ScriptParams datum' redeemer') ->
  WithExport datum' redeemer' info a ->
  WithExport datum redeemer info a
localScriptParams' f = localScriptParams (const f)

{- | As 'localScriptParams', but temporarily /setting/ the 'ScriptParams' in the
 scope instead. We still take the metadata parameter.

 @since 1.2.1
-}
setScriptParams ::
  forall (datum :: Type) (datum' :: Type) (redeemer :: Type) (redeemer' :: Type) (info :: Type) (a :: Type).
  (info -> ScriptParams datum' redeemer') ->
  WithExport datum' redeemer' info a ->
  WithExport datum redeemer info a
setScriptParams f = localScriptParams (\information _ -> f information)

{- | As 'setScriptParams', but without using the metadata parameter.

 @since 1.2.1
-}
setScriptParams' ::
  forall (datum :: Type) (datum' :: Type) (redeemer :: Type) (redeemer' :: Type) (info :: Type) (a :: Type).
  ScriptParams datum' redeemer' ->
  WithExport datum' redeemer' info a ->
  WithExport datum redeemer info a
setScriptParams' x = localScriptParams (\_ _ -> x)

{- | As 'localScriptParams', but modifying only the context. Provided for
 convenience. The other similarly-named functions are parallels to their
 'ScriptParams' equivalents.

 @since 1.2.1
-}
localScriptContext ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type) (a :: Type).
  (info -> ScriptContext -> ScriptContext) ->
  WithExport datum redeemer info a ->
  WithExport datum redeemer info a
localScriptContext f = localScriptParams (over #context . f)

-- | @since 1.2.1
localScriptContext' ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type) (a :: Type).
  (ScriptContext -> ScriptContext) ->
  WithExport datum redeemer info a ->
  WithExport datum redeemer info a
localScriptContext' f = localScriptContext (const f)

-- | @since 1.2.1
setScriptContext ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type) (a :: Type).
  (info -> ScriptContext) ->
  WithExport datum redeemer info a ->
  WithExport datum redeemer info a
setScriptContext f = localScriptContext (\information _ -> f information)

-- | @since 1.2.1
setScriptContext' ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type) (a :: Type).
  ScriptContext ->
  WithExport datum redeemer info a ->
  WithExport datum redeemer info a
setScriptContext' x = setScriptContext (const x)

{- | Given a name for the test collection, some \'initial parameters\' for the
 scripts being tested, and a pre-linked 'ScriptExport', build the tests
 specified.

 This function is similar in use to 'testGroup', and can be seen as a drop-in
 replacement for it:

 > main :: IO ()
 > main = defaultMain . withLinked "my tests" initialParams exports $ do
 >          assertMintingPolicy Crashes "unfinished" "aPolicy"
 >          localScriptParams tweakParams $ do
 >            ....

 @since 1.2.1
-}
withLinked ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type).
  String ->
  ScriptParams datum redeemer ->
  ScriptExport info ->
  WithExport datum redeemer info () ->
  TestTree
withLinked name sp se (WithExport comp) =
  let env = Env sp se
   in case execRWS comp env () of
        ((), TestMap result) ->
          testGroup name . toList . Map.foldMapWithKey go $ result
  where
    go ::
      Text ->
      PreparedScriptTests ->
      Acc TestTree
    go scriptName =
      pure . testGroup (unpack scriptName) . \case
        NoSuchScript -> [singleTest "Presence" MissingScript]
        PreparedTests rigs -> toList $ rigToTest <$> rigs

{- | As 'withLinked', but instead of a pre-linked 'ScriptExport', requires a
 'RawScriptExport' and a linker. Parameters for the linker can be provided via
 'TestParams'.

 = Note

 If the linking fails, you will get a single, automatically-failing, test that
 emits the linker error.

 @since 1.2.1
-}
withRaw ::
  forall (datum :: Type) (redeemer :: Type) (info :: Type) (params :: Type).
  String ->
  TestParams datum redeemer params ->
  RawScriptExport ->
  Linker params (ScriptExport info) ->
  WithExport datum redeemer info () ->
  TestTree
withRaw name tp raw linker comp =
  case runLinker linker raw (view #linkerParams tp) of
    Left err -> testGroup name [singleTest "linking" . FailedToLink $ err]
    Right linked -> withLinked name (view #scriptParams tp) linked comp

-- Helpers

newtype FailedToLink = FailedToLink LinkerError

instance IsTest FailedToLink where
  run _ (FailedToLink err) _ =
    pure . testFailed $
      "Linking failed\n"
        <> "Error:\n"
        <> show err
  testOptions = Tagged []

rigToTest :: TestRig -> TestTree
rigToTest = \case
  WrongRole label expected ->
    singleTest label . GotRoleWrong $ expected
  Prepared label outcome prep ->
    singleTest label . GotRoleRight outcome $ prep

data MissingScript = MissingScript

instance IsTest MissingScript where
  run _ MissingScript _ = pure . testFailed $ "No script exported with that name"
  testOptions = Tagged []

data Rigged
  = GotRoleWrong ScriptRole
  | GotRoleRight ScriptOutcome PreparedTest

instance IsTest Rigged where
  run _ rigged _ = case rigged of
    GotRoleWrong expected -> pure . testFailed $ case expected of
      ValidatorRole -> "Expected a validator, but given a minting policy"
      MintingPolicyRole -> "Expected a minting policy, but given a validator"
    GotRoleRight outcome prepped -> do
      let d = view #datum prepped
      let sc = view #context prepped
      let script = view #script prepped
      let applied = case preview #redeemer prepped of
            -- We have a minting policy
            Nothing -> applyArguments script [d, sc]
            -- We have a validator
            Just r -> applyArguments script [d, r, sc]
      let (res, _, logs) = evalScriptHuge applied
      pure $ case (res, outcome) of
        (Left _, Crashes) -> testPassed "Crashed as expected"
        (Left err, Runs) ->
          testFailed $
            "Expected to run, but crashed instead\n"
              <> "Error:\n"
              <> show err
              <> "\nLogs:\n"
              <> show logs
        (Right _, Crashes) ->
          testFailed $
            "Expected a crash, but ran instead\n"
              <> "Logs:\n"
              <> show logs
        (Right _, Runs) -> testPassed "Ran as expected"
  testOptions = Tagged []

-- Carries the environment for script test construction
data Env (datum :: Type) (redeemer :: Type) (info :: Type)
  = Env (ScriptParams datum redeemer) (ScriptExport info)

instance
  (k ~ A_Lens, a ~ ScriptParams datum redeemer, b ~ ScriptParams datum' redeemer') =>
  LabelOptic "params" k (Env datum redeemer info) (Env datum' redeemer' info) a b
  where
  labelOptic = lens out $ \(Env _ se) sp' -> Env sp' se
    where
      out :: Env datum redeemer info -> ScriptParams datum redeemer
      out (Env sp _) = sp

instance
  (k ~ A_Lens, a ~ ScriptExport info, b ~ ScriptExport info) =>
  LabelOptic "exports" k (Env datum redeemer info) (Env datum redeemer info) a b
  where
  labelOptic = lens out $ \(Env sp _) se' -> Env sp se'
    where
      out :: Env datum redeemer info -> ScriptExport info
      out (Env _ se) = se

-- A script with 'burned in' parameters
data PreparedTest
  = PreparedValidator Script Data Data Data
  | PreparedMintingPolicy Script Data Data

instance
  (k ~ A_Lens, a ~ Script, b ~ Script) =>
  LabelOptic "script" k PreparedTest PreparedTest a b
  where
  labelOptic = lens out $ \prep script' -> case prep of
    PreparedValidator _ d r sc -> PreparedValidator script' d r sc
    PreparedMintingPolicy _ d sc -> PreparedMintingPolicy script' d sc
    where
      out :: PreparedTest -> Script
      out = \case
        PreparedValidator s _ _ _ -> s
        PreparedMintingPolicy s _ _ -> s

instance
  (k ~ A_Lens, a ~ Data, b ~ Data) =>
  LabelOptic "datum" k PreparedTest PreparedTest a b
  where
  labelOptic = lens out $ \prep d' -> case prep of
    PreparedValidator s _ r sc -> PreparedValidator s d' r sc
    PreparedMintingPolicy s _ sc -> PreparedMintingPolicy s d' sc
    where
      out :: PreparedTest -> Data
      out = \case
        PreparedValidator _ d _ _ -> d
        PreparedMintingPolicy _ d _ -> d

instance
  (k ~ A_Lens, a ~ Data, b ~ Data) =>
  LabelOptic "context" k PreparedTest PreparedTest a b
  where
  labelOptic = lens out $ \prep sc' -> case prep of
    PreparedValidator s d r _ -> PreparedValidator s d r sc'
    PreparedMintingPolicy s d _ -> PreparedMintingPolicy s d sc'
    where
      out :: PreparedTest -> Data
      out = \case
        PreparedValidator _ _ _ sc -> sc
        PreparedMintingPolicy _ _ sc -> sc

instance
  (k ~ An_AffineTraversal, a ~ Data, b ~ Data) =>
  LabelOptic "redeemer" k PreparedTest PreparedTest a b
  where
  labelOptic = atraversal out $ \prep r' -> case prep of
    PreparedValidator s d _ sc -> PreparedValidator s d r' sc
    PreparedMintingPolicy s d sc -> PreparedMintingPolicy s d sc
    where
      out :: PreparedTest -> Either PreparedTest Data
      out = \case
        PreparedValidator _ _ r _ -> Right r
        prep@PreparedMintingPolicy {} -> Left prep

-- Either a 'burned in' script, or an indication the role was wrong
data TestRig
  = WrongRole String ScriptRole
  | Prepared String ScriptOutcome PreparedTest

data PreparedScriptTests
  = NoSuchScript
  | PreparedTests (Acc TestRig)

instance Semigroup PreparedScriptTests where
  NoSuchScript <> _ = NoSuchScript
  _ <> NoSuchScript = NoSuchScript
  PreparedTests xs <> PreparedTests xs' = PreparedTests $ xs <> xs'

newtype TestMap = TestMap (Map Text PreparedScriptTests)

instance Semigroup TestMap where
  TestMap m <> TestMap m' = TestMap . Map.unionWith (<>) m $ m'

instance Monoid TestMap where
  mempty = TestMap []

noSuchScript :: Text -> TestMap
noSuchScript name = TestMap [(name, NoSuchScript)]

wrongRole :: Text -> String -> ScriptRole -> TestMap
wrongRole name reason expected =
  TestMap [(name, PreparedTests [WrongRole reason expected])]

prepareValidator ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum, ToData redeemer) =>
  Text ->
  String ->
  Script ->
  ScriptOutcome ->
  ScriptParams datum redeemer ->
  TestMap
prepareValidator name reason script expected sp =
  let datum = toData . view #datum $ sp
      redeemer = toData . view #redeemer $ sp
      sc = toData . view #context $ sp
      prepped = PreparedValidator script datum redeemer sc
   in TestMap
        [ (name, PreparedTests [Prepared reason expected prepped])
        ]

prepareMintingPolicy ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum) =>
  Text ->
  String ->
  Script ->
  ScriptOutcome ->
  ScriptParams datum redeemer ->
  TestMap
prepareMintingPolicy name reason script expected sp =
  let datum = toData . view #datum $ sp
      sc = toData . view #context $ sp
      prepped = PreparedMintingPolicy script datum sc
   in TestMap
        [ (name, PreparedTests [Prepared reason expected prepped])
        ]
