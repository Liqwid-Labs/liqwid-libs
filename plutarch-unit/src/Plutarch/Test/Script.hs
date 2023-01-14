{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Tasty provider for testing 'ScriptCase'.

 @since 1.3
-}
module Plutarch.Test.Script (
  -- * High level
  ScriptCase (..),
  ScriptResult (..),
  testScript,
  testScriptGroup,

  -- * Low level functions
  runScript,
) where

import GHC.Generics (Generic)

--------------------------------------------------------------------------------

import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Test.Tasty.Providers (
  IsTest (
    run,
    testOptions
  ),
  TestTree,
  singleTest,
  testFailed,
  testPassed,
 )

--------------------------------------------------------------------------------

import Plutarch.Evaluate (evalScriptHuge)
import Plutarch.Script (Script)
import Test.Tasty (testGroup)

--------------------------------------------------------------------------------

-- | @since 1.3
data ScriptResult
  = -- | @since 1.3
    ScriptSuccess
  | -- | @since 1.3
    ScriptFailure
  deriving stock
    ( -- | @since 1.3
      Eq
    , -- | @since 1.3
      Show
    )

{- | Full script info for testing.

 @since 1.3
-}
data ScriptCase = ScriptCase
  { name :: String
  -- ^ The name.
  --
  -- @since 1.3
  , expectation :: ScriptResult
  -- ^ The expectation.
  --
  -- @since 1.3
  , script :: Script
  -- ^ The script.
  --
  -- @since 1.3
  , debugScript :: Script
  -- ^ Debug version of the script for .
  --
  -- @since 1.3
  }
  deriving stock
    ( -- | @since 1.3
      Eq
    , -- | @since 1.3
      Generic
    , -- | @since 1.3
      Show
    )

-- | @since 1.3
makeFieldLabelsNoPrefix ''ScriptCase

-- | @since 1.3
instance IsTest ScriptCase where
  testOptions = Tagged []
  run _options sc _progress = do
    case (view #expectation sc, runScriptCase sc) of
      -- expected success, received failure
      (ScriptSuccess, (ScriptFailure, msg)) -> pure $ testFailed msg
      -- expected failure, received success
      (ScriptFailure, (ScriptSuccess, msg)) -> pure $ testFailed msg
      _ -> pure $ testPassed ""

{- | Turns a 'ScriptCase' into a 'TestTree' using its 'name'.

 @since 1.3
-}
testScript :: ScriptCase -> TestTree
testScript sc = singleTest (view #name sc) sc

{- | 'testGroup' but for 'ScriptCase'.

 @since 1.3
-}
testScriptGroup :: String -> [ScriptCase] -> TestTree
testScriptGroup desc scs = testGroup desc $ testScript <$> scs

{- | Low-level function for running a 'ScriptCase'.

 @since 1.3
-}
runScriptCase :: ScriptCase -> (ScriptResult, String)
runScriptCase sc =
  runScript
    (view #script sc)
    (view #debugScript sc)
    msg
  where
    msg = case view #expectation sc of
      ScriptSuccess -> ""
      ScriptFailure -> "Expected failure, but script succeeded"

{- | Low-level function for running a script.

 @since 1.3
-}
runScript ::
  -- | Script to run.
  Script ->
  -- | Debug version of the script.
  Script ->
  -- | Message to return upon success.
  String ->
  -- | Returns the result of evaluating the script, along with the parameter
  -- message upon success, or an error message upon failure.
  (ScriptResult, String)
runScript script debug onSuccess = case scriptResult of
  (Right _, _, _) -> (ScriptSuccess, onSuccess)
  (Left err, _, _) -> (ScriptFailure, showError dTrace (show err))
  where
    scriptResult = evalScriptHuge script
    (_, _, dTrace) = evalScriptHuge debug

showError :: [Text] -> String -> String
showError traces err =
  "Script failed with error: "
    <> err
    <> "\nTrace Log:\n"
    <> T.unpack (T.intercalate "\n" traces)
