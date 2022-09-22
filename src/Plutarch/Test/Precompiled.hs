{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
) where

import Acc (Acc)
import Control.Monad.RWS (
  MonadReader,
  MonadWriter,
  RWS,
  ask,
  execRWS,
  local,
  tell
 )
import Data.Foldable (toList)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Extra.DebuggableScript (
  DebuggableScript (DebuggableScript, debugScript, script),
  applyDebuggableScript,
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
    deriving stock ( -- | @since 1.1.0
                     Show)

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

ourStyle :: Style
ourStyle = style{lineLength = 80}

ppLogs :: [Text] -> Doc
ppLogs = \case
    [] -> "No logs found. Did you forget to build with +development?"
    logs -> vcat $ ppDoc <$> logs

failWithStyle :: Doc -> Result
failWithStyle = testFailed . renderStyle ourStyle

instance IsTest TestCase where
    testOptions = Tagged []
    run _ EvalTestCase{..} _ = return $
        case (r, expectation) of
            (Right _, Success) -> testPassed ""
            (Right x, Failure) -> failWithStyle . unexpectedSuccess x $ dt
            (Left err, Success) -> failWithStyle . unexpectedFailure err $ dt
            (Left _, Failure) -> testPassed ""
      where
        ((r :: Either EvalError Script), _, _) = evalScript $ script dScript
        (_, _, (dt :: [Text])) = evalScript $ debugScript dScript
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
    run _ EqualityTestCase{..} _ = return $
        case (r, e) of
            (Right x, Right y) ->
                if x == y
                    then testPassed ""
                    else testFailed "Two script does not match"
            (Left err, _) -> failWithStyle . failedToEvaluate err $ dt
            (_, Left err) -> failWithStyle . failedToEvaluate err $ mempty
      where
        ((r :: Either EvalError Script), _, _) = evalScript $ script dScript
        ((e :: Either EvalError Script), _, _) = evalScript $ expectedScript
        (_, _, (dt :: [Text])) = evalScript $ debugScript dScript
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
        ( Functor
        , Applicative
        , Monad
        , MonadReader DebuggableScript
        , MonadWriter (Acc TestCase)
        )
        via (RWS DebuggableScript (Acc TestCase) ())

{- | Stitches in arguments. It is helpful if there are shared arguments.

 @since 1.1.0
-}
withApplied :: [Data] -> TestCompiled () -> TestCompiled ()
withApplied args tests = local (flip applyDebuggableScript args) tests

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

 @since 1.1.0
-}
fromPTerm ::
    forall (a :: S -> Type).
    String ->
    ClosedTerm a ->
    TestCompiled () ->
    TestTree
fromPTerm name term ctests =
  fromScript name s ds ctests
  where
    (DebuggableScript s ds) = mustCompileD term

{- | Run some tests using given script and debug script in 'TestCompiled'.

 @since 1.1.0
-}
fromScript ::
  String ->
  Script ->
  Script ->
  TestCompiled () ->
  TestTree
fromScript name script debugScript ctests =
    testGroup name $ toList $ go <$> tests
  where
    (_, tests) = execRWS (unTestCompiled ctests) (DebuggableScript script debugScript) ()
    go ts = singleTest (caseName ts) ts
