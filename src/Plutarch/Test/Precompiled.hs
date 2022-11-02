{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module: Plutarch.Test.Precompiled
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Seungheon Oh <seungheon@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A 'TestTree' whose tests make use of a shared precompiled script.
-}
module Plutarch.Test.Precompiled (
  Expectation (..),
  TestCase,
  (@&),
  withApplied,
  TestCompiled,
  testEvalCase,
  (@>),
  (@!>),
  testEqualityCase,
  fromPTerm,
  tryFromPTerm,
) where

import Acc (Acc)
import Control.Monad.RWS (
  MonadReader,
  MonadWriter,
  RWS,
  ask,
  execRWS,
  local,
  tell,
 )
import Data.Foldable (toList)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Getter (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Extra.DebuggableScript (
  DebuggableScript,
  applyDebuggableScript,
  checkedCompileD,
  mustCompileD,
 )
import Plutarch.Prelude (ClosedTerm, S, Type)
import PlutusLedgerApi.V1.Scripts (Script)
import PlutusLedgerApi.V2 (Data)
import Test.Tasty (testGroup)
import Test.Tasty.Providers (IsTest, Result, TestTree, run, singleTest, testFailed, testOptions, testPassed)
import Text.PrettyPrint (
  Doc,
  Style,
  hang,
  lineLength,
  renderStyle,
  style,
  vcat,
 )
import Text.Show.Pretty (ppDoc)

-- | @since 1.1.0
data Expectation
  = Success
  | Failure
  deriving stock
    ( -- | @since 1.1.0
      Show
    )

{- | Holds necessary information for each test cases.

 @since 1.1.0
-}
data TestCase
  = EvalTestCase
      { dScript :: DebuggableScript
      , caseName :: String
      , expectation :: Expectation
      }
  | EqualityTestCase
      { dScript :: DebuggableScript
      , expectedScript :: Script
      , caseName :: String
      }
  -- @since 1.2.0
  deriving stock (Generic)

ourStyle :: Style
ourStyle = style {lineLength = 80}

ppLogs :: [Text] -> Doc
ppLogs = \case
  [] -> "No logs found. Did you forget to build with +development?"
  logs -> vcat $ ppDoc <$> logs

failWithStyle :: Doc -> Result
failWithStyle = testFailed . renderStyle ourStyle

instance IsTest TestCase where
  testOptions = Tagged []
  run _ EvalTestCase {..} _ = return $
    case (r, expectation) of
      (Right _, Success) -> testPassed ""
      (Right x, Failure) -> failWithStyle . unexpectedSuccess x $ dt
      (Left err, Success) -> failWithStyle . unexpectedFailure err $ dt
      (Left _, Failure) -> testPassed ""
    where
      (r :: Either EvalError Script, _, _) = evalScript $ view #script dScript
      (_, _, dt :: [Text]) = evalScript $ view #debugScript dScript
      unexpectedFailure :: EvalError -> [Text] -> Doc
      unexpectedFailure err logs =
        "Expected a successful run, but failed instead.\n"
          <> hang "Error" 4 (ppDoc err)
          <> hang "Logs" 4 (ppLogs logs)
      unexpectedSuccess :: Script -> [Text] -> Doc
      unexpectedSuccess result logs =
        "Expected a failing run, but succeeded instead.\n"
          <> hang "Result" 4 (ppDoc result)
          <> hang "Logs" 4 (ppLogs logs)
  run _ EqualityTestCase {..} _ = return $
    case (r, e) of
      (Right x, Right y) ->
        if x == y
          then testPassed ""
          else testFailed "Two script does not match"
      (Left err, _) -> failWithStyle . failedToEvaluate err $ dt
      (_, Left err) -> failWithStyle . failedToEvaluate err $ mempty
    where
      (r :: Either EvalError Script, _, _) = evalScript $ view #script dScript
      (e :: Either EvalError Script, _, _) = evalScript expectedScript
      (_, _, dt :: [Text]) = evalScript $ view #debugScript dScript
      failedToEvaluate :: EvalError -> [Text] -> Doc
      failedToEvaluate result logs =
        "Script evaluation failed, both scripts need to suceed in order to check equality.\n"
          <> hang "Result" 4 (ppDoc result)
          <> hang "Logs" 4 (ppLogs logs)

{- | Allows monadically defining a 'TestTree' whose tests all use the same precompiled script.

 @since 1.1.0
-}
newtype TestCompiled a = TestCompiled
  { unTestCompiled ::
      RWS DebuggableScript (Acc TestCase) () a
  }
  deriving
    ( -- | @since 1.1.0
      Functor
    , -- | @since 1.1.0
      Applicative
    , -- | @since 1.1.0
      Monad
    , -- | @since 1.1.0
      MonadReader DebuggableScript
    , -- | @since 1.1.0
      MonadWriter (Acc TestCase)
    )
    via (RWS DebuggableScript (Acc TestCase) ())
  -- @since 1.2.0
  deriving stock (Generic)

{- | Stitches in arguments. It is helpful if there are shared arguments.

 @since 1.1.0
-}
withApplied :: [Data] -> TestCompiled () -> TestCompiled ()
withApplied args = local (`applyDebuggableScript` args)

{- | An operator for 'withApplied'.

 @since 1.1.0
-}
(@&) :: [Data] -> TestCompiled () -> TestCompiled ()
args @& tests = withApplied args tests

infixr 1 @&

{- | Tests if a script succeeded or not, given the specified arguments.

 @since 1.1.0
-}
testEqualityCase :: String -> Script -> [Data] -> TestCompiled ()
testEqualityCase name e args = do
  ds <- ask
  let applied = applyDebuggableScript ds args
      testCase =
        EqualityTestCase
          { dScript = applied
          , expectedScript = e
          , caseName = name
          }
  tell $ pure testCase
  return ()

{- | Tests if script succeed or not given arguments.

 @since 1.1.0
-}
testEvalCase :: String -> Expectation -> [Data] -> TestCompiled ()
testEvalCase name e args = do
  ds <- ask
  let applied = applyDebuggableScript ds args
      testCase =
        EvalTestCase
          { dScript = applied
          , caseName = name
          , expectation = e
          }
  tell $ pure testCase
  return ()

{- | An operator for 'testEvalCase'.

 @since 1.1.0
-}
(@>) :: [Data] -> String -> TestCompiled ()
args @> name = testEvalCase name Success args

infixr 1 @>

{- | An operator for 'testEvalCase'.

 @since 1.1.0
-}
(@!>) :: [Data] -> String -> TestCompiled ()
args @!> name = testEvalCase name Failure args

infixr 1 @!>

{- | Compiles a Plutarch closed term, then runs some tests using it in 'TestCompiled'.
Returns an error message if compilation fails

 @since 1.2.0
-}
fromPTerm ::
  forall (a :: S -> Type).
  String ->
  ClosedTerm a ->
  TestCompiled () ->
  Either Text TestTree
fromPTerm name term ctests = case checkedCompileD term of
  Left err -> Left err
  Right debuggableScript ->
    let (_, tests) = execRWS (view #unTestCompiled ctests) debuggableScript ()
        go ts = singleTest (view #caseName ts) ts
     in Right $ testGroup name $ toList $ go <$> tests

{- | Compiles a Plutarch closed term, then runs some tests using it in 'TestCompiled'.
Errors if compilation fails

 @since 1.2.0
-}
tryFromPTerm ::
  forall (a :: S -> Type).
  String ->
  ClosedTerm a ->
  TestCompiled () ->
  TestTree
tryFromPTerm name term ctests =
  testGroup name $ toList $ go <$> tests
  where
    (_, tests) = execRWS (view #unTestCompiled ctests) (mustCompileD term) ()
    go ts = singleTest (view #caseName ts) ts

-- | @since 1.2.0
makeFieldLabelsNoPrefix ''TestCase

-- | @since 1.2.0
makeFieldLabelsNoPrefix ''TestCompiled
