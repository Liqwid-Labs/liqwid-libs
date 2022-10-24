{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.DebuggableScript (
  -- * Type
  DebuggableScript,

  -- * Construction
  checkedCompileD,
  mustCompileD,

  -- * Use
  applyScript,
  applyDebuggableScript,
  applyDebuggableArg,
  mustFinalEvalDebuggableScript,
  finalEvalDebuggableScript,
  mustEvalScript,
  mustEvalD,
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics.Getter (A_Getter, to, view)
import Optics.Label (LabelOptic (labelOptic))
import Plutarch (
  Config (Config, tracingMode),
  TracingMode (DetTracing, NoTracing),
  compile,
 )
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Extra.Compile (mustCompile, mustCompileTracing)
import PlutusLedgerApi.V1 (Data, ExBudget, Script)
import PlutusLedgerApi.V1.Scripts (Script (Script), applyArguments)
import UntypedPlutusCore (
  Program (
    Program,
    _progAnn,
    _progTerm,
    _progVer
  ),
 )
import qualified UntypedPlutusCore.Core.Type as UplcType
import UntypedPlutusCore.Evaluation.Machine.Cek (
  CekUserError (CekEvaluationFailure, CekOutOfExError),
  ErrorWithCause (ErrorWithCause),
  EvaluationError (InternalEvaluationError, UserEvaluationError),
 )

{- | A 'Script' with a debug fallback that has tracing turned on.

 @since 3.8.0
-}
data DebuggableScript = DebuggableScript Script Script
  deriving stock
    ( -- | @since 3.0.2
      Eq
    , -- | @since 3.0.2
      Show
    , -- | @since 3.0.2
      Generic
    )
  deriving anyclass
    ( -- | @since 3.0.2
      NFData
    )

{- | Retrieves the non-debugging 'Script'. This is read-only, as allowing it to
 change could break invariants.

 @since 3.8.0
-}
instance
  (k ~ A_Getter, a ~ Script, b ~ Script) =>
  LabelOptic "script" k DebuggableScript DebuggableScript a b
  where
  labelOptic = to $ \(DebuggableScript x _) -> x

{- | Retrieves the debugging 'Script'. This is read-only, as allowing it to
 change could break invariants.

 @since 3.8.0
-}
instance
  (k ~ A_Getter, a ~ Script, b ~ Script) =>
  LabelOptic "debugScript" k DebuggableScript DebuggableScript a b
  where
  labelOptic = to $ \(DebuggableScript _ x) -> x

{- | Apply a function to an argument on the compiled 'Script' level.

 @since 3.8.0
-}
applyScript :: Script -> Script -> Script
applyScript f a =
  if fVer /= aVer
    then error "apply: Plutus Core version mismatch"
    else
      Script
        Program
          { _progTerm = UplcType.Apply () fTerm aTerm
          , _progVer = fVer
          , _progAnn = ()
          }
  where
    (Script Program {_progTerm = fTerm, _progVer = fVer}) = f
    (Script Program {_progTerm = aTerm, _progVer = aVer}) = a

{- | Apply given arguments to 'DebuggableScript'.

 @since 3.7.1
-}
applyDebuggableScript :: DebuggableScript -> [Data] -> DebuggableScript
applyDebuggableScript (DebuggableScript script debugScript) args =
  DebuggableScript (applyArguments script args) (applyArguments debugScript args)

{- | Apply a single argument, provided as a 'DebuggableScript'.

 @since 3.8.0
-}
applyDebuggableArg ::
  DebuggableScript ->
  DebuggableScript ->
  DebuggableScript
applyDebuggableArg f x =
  DebuggableScript
    (applyScript (view #script f) (view #script x))
    (applyScript (view #debugScript f) (view #debugScript x))

{- | For handling compilation errors right away.

 You pay for the compilation of the debug script, even if it's not needed down
 the line. You most likely want 'mustCompileD' instead.

  @since 3.0.2
-}
checkedCompileD ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either Text DebuggableScript
checkedCompileD term = do
  script <- compile Config {tracingMode = NoTracing} term
  debugScript <- compile Config {tracingMode = DetTracing} term
  pure $ DebuggableScript script debugScript

{- | Compilation errors cause exceptions, but deferred by lazyness.

 You don't pay for compilation of the debug script if it's not needed!

 @since 3.0.2
-}
mustCompileD ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  DebuggableScript
mustCompileD term =
  DebuggableScript (mustCompile term) (mustCompileTracing term)

{- | Final evaluation of a 'DebuggableScript' to a 'Script', with errors resulting in
 exceptions.

 @since 3.0.2
-}
mustFinalEvalDebuggableScript :: DebuggableScript -> Script
mustFinalEvalDebuggableScript s =
  let (res, _, traces) = finalEvalDebuggableScript s
   in case res of
        Right r -> r
        Left err ->
          error $
            unlines
              [ "Error when evaluating Script:"
              , show err
              , "Traces:"
              , Text.unpack (Text.unlines traces)
              ]

{- | Final evaluation of a 'DebuggableScript', with full 'evalScript' result.

 Falls back to the debug script if a 'UserEvaluationError' occurs. Verifies that
 the debug script results in a 'UserEvaluationError' too, throws an exception
 otherwise.

 @since 3.0.2
-}
finalEvalDebuggableScript ::
  DebuggableScript ->
  (Either EvalError Script, ExBudget, [Text])
finalEvalDebuggableScript (DebuggableScript script debugScript) =
  case res of
    Right _ -> r
    Left (ErrorWithCause evalErr _) ->
      case evalErr of
        UserEvaluationError e ->
          case e of
            CekEvaluationFailure ->
              verifyDebuggableScriptOutput evalErr
            _ -> r
        _ -> r
  where
    r@(res, _, _) = evalScript script
    r'@(res', _, traces) = evalScript debugScript
    verifyDebuggableScriptOutput origEvalErr =
      case res' of
        Right _ ->
          error $
            unlines
              [ "Script failed, but corresponding debug Script "
                  <> "succeeded!"
              , "Original error: "
              , show origEvalErr
              , "Debug Script traces:"
              , Text.unpack (Text.unlines traces)
              ]
        Left (ErrorWithCause evalErr _) ->
          case evalErr of
            UserEvaluationError e ->
              case e of
                CekEvaluationFailure ->
                  r'
                CekOutOfExError _ ->
                  error $
                    unlines
                      [ "Script failed normally, "
                          <> "but corresponding debug Script"
                          <> "ran out of budget!"
                      , "Original error:"
                      , show origEvalErr
                      , "Debug Script traces until crash:"
                      , Text.unpack (Text.unlines traces)
                      ]
            InternalEvaluationError e ->
              error $
                unlines
                  [ "Script failed with UserEvaluationError, "
                      <> "but corresponding debug Script caused "
                      <> "internal evaluation error!"
                  , "an Internal evaluation error:"
                  , show e
                  , "Original error:"
                  , show origEvalErr
                  , "Debug Script traces until crash:"
                  , Text.unpack (Text.unlines traces)
                  ]

{- | Evaluate a 'Script' to a 'Script', with errors resulting in exceptions.

 This is mostly useful for pre-evaluating arguments to a thing being
 tested/benchmarked.

 @since 3.0.2
-}
mustEvalScript :: Script -> Script
mustEvalScript s =
  case res of
    Left err ->
      error $
        unlines
          [ "Error when evaluating Script:"
          , show err
          , "Traces:"
          , Text.unpack (Text.unlines traces)
          ]
    Right sr -> sr
  where
    (res, _, traces) = evalScript s

{- | Evaluate a 'DebuggableScript' to a 'DebuggableScript', with errors
  resulting in exceptions.

 This is mostly useful for pre-evaluating arguments to a thing being
 tested/benchmarked.
 Lazyness defers the evaluation (and exception) until it's needed, so the debug
 script causes no unneccessary work.

 @since 3.0.2
-}
mustEvalD :: DebuggableScript -> DebuggableScript
-- - If something else tries to use 'script' and it fails, we must fall
--   back to 'debugScript', this is just what 'mustEvalDebuggableScript' does.
-- - If something tries to use 'debugScript' directly (because another
--   Script in some expression failed already), there is nothing to fall
--   back to, so we need only 'mustEvalScript'.
mustEvalD ds =
  DebuggableScript
    (mustFinalEvalDebuggableScript ds)
    (mustEvalScript . view #debugScript $ ds)
