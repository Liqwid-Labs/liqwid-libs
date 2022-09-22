{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Plutarch.Test.Precompiled (
    Expectation (..),
    TestCase,
    (&@),
    withApplied,
    TestCompiled (..),
    testEvalCase,
    (<--@),
    (<-!@),
    fromPTerm,
) where

import Acc (Acc)
import Control.Monad.RWS (MonadReader, MonadWriter, RWS, ask, execRWS, local, tell)
import Data.Foldable (toList)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Extra.DebuggableScript (DebuggableScript (DebuggableScript, debugScript, script), mustCompileD)
import Plutarch.Prelude (ClosedTerm, S, Type)
import PlutusLedgerApi.V1.Scripts (Script, applyArguments)
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

applyDebuggableScript :: DebuggableScript -> [Data] -> DebuggableScript
applyDebuggableScript (DebuggableScript script debugScript) d =
    DebuggableScript
        { script = applyArguments script d
        , debugScript = applyArguments debugScript d
        }

data Expectation
    = Success
    | Failure
    deriving stock (Show)

data TestCase = TestCase
    { dScript :: DebuggableScript
    , caseName :: String
    , expectation :: Expectation
    }

ourStyle :: Style
ourStyle = style{lineLength = 80}

ppLogs :: [Text] -> Doc
ppLogs = \case
    [] -> "No logs found. Did you forget to build with +development?"
    logs -> vcat $ ppDoc <$> logs

instance IsTest TestCase where
    testOptions = Tagged []
    run _ TestCase{..} _ = return $
        case (r, expectation) of
            (Right _, Success) -> testPassed ""
            (Right x, Failure) -> failWithStyle . unexpectedSuccess x $ dt
            (Left err, Success) -> failWithStyle . unexpectedFailure err $ dt
            (Left _, Failure) -> testPassed ""
      where
        (r, _, _) = evalScript $ script dScript
        (_, _, dt) = evalScript $ debugScript dScript
        failWithStyle :: Doc -> Result
        failWithStyle = testFailed . renderStyle ourStyle
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

withApplied :: [Data] -> TestCompiled () -> TestCompiled ()
withApplied args tests = local (flip applyDebuggableScript args) tests

(&@) :: [Data] -> TestCompiled () -> TestCompiled ()
args &@ tests = withApplied args tests

testEvalCase :: String -> Expectation -> [Data] -> TestCompiled ()
testEvalCase name e args = do
    ds <- ask
    let applied = applyDebuggableScript ds args
        testCase =
            TestCase
                { dScript = applied
                , caseName = name
                , expectation = e
                }
    tell $ pure testCase
    return ()

(<--@) :: String -> [Data] -> TestCompiled ()
name <--@ args = testEvalCase name Success args

(<-!@) :: String -> [Data] -> TestCompiled ()
name <-!@ args = testEvalCase name Failure args

fromPTerm ::
    forall (a :: S -> Type).
    String ->
    ClosedTerm a ->
    TestCompiled () ->
    TestTree
fromPTerm name term ctests =
    testGroup name $ toList $ go <$> tests
  where
    dScript = mustCompileD term
    (_, tests) = execRWS (unTestCompiled ctests) dScript ()
    go ts = singleTest (caseName ts) ts
