{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

{- | Module: Plutarch.Test.Unit
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Seungheon Oh <seungheon@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Small unit test for Plutarch that does not require extra testing
 library like hunit.
-}
module Plutarch.Test.Unit (
  -- * Validator tests
  validatorSucceedsWith,
  validatorFailsWith,

  -- * Minting policy tests
  mintingPolicySucceedsWith,
  mintingPolicyFailsWith,

  -- * Lower-level functionality
  scriptSucceeds,
  scriptFails,
) where

import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutarch.Api.V2 (PMintingPolicy, PValidator)
import Plutarch.Builtin (pforgetData)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Extra.Compile (mustCompile)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude (
  PData,
  S,
  Term,
  Type,
  pconstant,
  pconstantData,
  (#),
 )
import PlutusLedgerApi.V2 (Script, ScriptContext)
import PlutusTx.IsData.Class (ToData)
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
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

{- | Ensure the given 'Script' executes without erroring.
 This is a low-level function: you probably don't need this, unless you're
 implementing something yourself.

 @since 1.0.0
-}
scriptSucceeds :: String -> Script -> TestTree
scriptSucceeds name = singleTest name . ExpectSuccess

{- | Ensure the given 'Script' errors during its execution.
 This is a low-level function: you probably don't need this, unless you're
 implementing something yourself.

 @since 1.0.0
-}
scriptFails :: String -> Script -> TestTree
scriptFails name = singleTest name . ExpectFailure

{- | Ensure the given minting policy executes without erroring, given the
 specified input redeemer.

 @since 1.0.0
-}
mintingPolicySucceedsWith ::
  forall (redeemer :: S -> Type).
  (PUnsafeLiftDecl redeemer, ToData (PLifted redeemer)) =>
  String ->
  (forall (s :: S). Term s PMintingPolicy) ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
mintingPolicySucceedsWith = testMP scriptSucceeds

{- | Ensure the given minting policy errors during its execution, given the
 specified input redeemer.

 @since 1.0.0
-}
mintingPolicyFailsWith ::
  forall (redeemer :: S -> Type).
  (PUnsafeLiftDecl redeemer, ToData (PLifted redeemer)) =>
  String ->
  (forall (s :: S). Term s PMintingPolicy) ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
mintingPolicyFailsWith = testMP scriptFails

{- | Ensure the given validator executes without erroring, given the specified
 input datum and redeemer.

 @since 1.0.0
-}
validatorSucceedsWith ::
  forall (datum :: S -> Type) (redeemer :: S -> Type).
  ( PUnsafeLiftDecl datum
  , PUnsafeLiftDecl redeemer
  , ToData (PLifted datum)
  , ToData (PLifted redeemer)
  ) =>
  String ->
  (forall (s :: S). Term s PValidator) ->
  PLifted datum ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
validatorSucceedsWith = testValidator scriptSucceeds

{- | Ensure the given validator errors during its execution, given the
 specified input datum and redeemer.

 @since 1.0.0
-}
validatorFailsWith ::
  forall (datum :: S -> Type) (redeemer :: S -> Type).
  ( PUnsafeLiftDecl datum
  , PUnsafeLiftDecl redeemer
  , ToData (PLifted datum)
  , ToData (PLifted redeemer)
  ) =>
  String ->
  (forall (s :: S). Term s PValidator) ->
  PLifted datum ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
validatorFailsWith = testValidator scriptFails

-- Helpers

testValidator ::
  forall (datum :: S -> Type) (redeemer :: S -> Type).
  ( PUnsafeLiftDecl datum
  , PUnsafeLiftDecl redeemer
  , ToData (PLifted datum)
  , ToData (PLifted redeemer)
  ) =>
  (String -> Script -> TestTree) ->
  String ->
  (forall (s :: S). Term s PValidator) ->
  PLifted datum ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
testValidator f name val dat red sc =
  f name $
    mustCompile
      ( val
          # pforgetConstant dat
          # pforgetConstant red
          # pconstant sc
      )

pforgetConstant ::
  forall (a :: S -> Type) (s :: S).
  (PUnsafeLiftDecl a, ToData (PLifted a)) =>
  PLifted a ->
  Term s PData
pforgetConstant x = pforgetData (pconstantData x)

testMP ::
  forall (redeemer :: S -> Type).
  (PUnsafeLiftDecl redeemer, ToData (PLifted redeemer)) =>
  (String -> Script -> TestTree) ->
  String ->
  (forall (s :: S). Term s PMintingPolicy) ->
  PLifted redeemer ->
  ScriptContext ->
  TestTree
testMP f name policy red sc =
  f name $
    mustCompile
      ( policy # pforgetConstant red # pconstant sc
      )

data ScriptTest = ExpectSuccess Script | ExpectFailure Script

getScript :: ScriptTest -> Script
getScript = \case
  ExpectSuccess s -> s
  ExpectFailure s -> s

instance IsTest ScriptTest where
  testOptions = Tagged []
  run _ st _ = do
    let script = getScript st
    let (res, _, logs) = evalScript script
    pure $ case (res, st) of
      (Left _, ExpectFailure _) -> testPassed ""
      (Right _, ExpectSuccess _) -> testPassed ""
      (Left err, ExpectSuccess _) -> failWithStyle . unexpectedFailure err $ logs
      (Right x, ExpectFailure _) -> failWithStyle . unexpectedSuccess x $ logs
    where
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

ourStyle :: Style
ourStyle = style {lineLength = 80}

ppLogs :: [Text] -> Doc
ppLogs = \case
  [] -> "No logs found. Did you forget to build with +development?"
  logs -> vcat $ ppDoc <$> logs
