{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Pre-compiling Plutarch functions and applying them.

 Speeds up benchmarking and testing.
-}
module Plutarch.Extra.Precompile (
  applyScript,
  -- Exporting the data constructor on purpose, since users might want to
  -- deserialize compiled terms.  If someone wants to subvert type safety using
  -- Scripts, they can do that regardless of this export.
  CompiledTerm (..),
  CompiledTerm' (..),
  compile',
  toDebuggableScript,
  applyCompiledTerm,
  applyCompiledTerm',
  applyCompiledTerm2,
  applyCompiledTerm2',
  (##),
  (##~),
  (###),
  (###~),
  pliftCompiled',
  pliftCompiled,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.DebuggableScript (
  DebuggableScript (DebuggableScript),
  debugScript,
  finalEvalDebuggableScript,
  mustCompileD,
  mustEvalD,
  script,
 )
import Plutarch.Internal (
  Config (Config),
  RawTerm (RCompiled),
  Term (Term),
  TermResult (TermResult),
  TracingMode (DetTracing),
  tracingMode,
 )
import Plutarch.Lift (
  LiftError (
    LiftError_CompilationError,
    LiftError_EvalError,
    LiftError_FromRepr,
    LiftError_KnownTypeError
  ),
  PUnsafeLiftDecl (PLifted),
  plift',
 )
import PlutusCore.Builtin (KnownTypeError (KnownTypeEvaluationFailure, KnownTypeUnliftingError))
import PlutusLedgerApi.V1.Scripts (Script (Script))
import UntypedPlutusCore (Program (Program, _progAnn, _progTerm, _progVer))
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Core.Type as UplcType

{- | Apply a function to an argument on the compiled 'Script' level.

 @since 3.0.2
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

applyDebuggableScript ::
  DebuggableScript ->
  DebuggableScript ->
  DebuggableScript
applyDebuggableScript f a =
  DebuggableScript
    { script = applyScript f.script a.script
    , debugScript = applyScript f.debugScript a.debugScript
    }

{- | Type-safe wrapper for compiled Plutarch functions.

 @since 3.0.2
-}
newtype CompiledTerm (a :: S -> Type) = CompiledTerm {debuggableScript :: DebuggableScript}

{- | Like 'CompiledTerm', but with the internal 'Script's re-packaged into 'Term's.

 @since 3.0.2
-}
data CompiledTerm' (a :: S -> Type) = CompiledTerm'
  { term :: forall (s :: S). Term s a
  , debugTerm :: forall (s :: S). Term s a
  }

{- | Compile a closed Plutarch 'Term' to a 'CompiledTerm'.

 Beware, the Script inside contains everything it needs. You can end up with
 multiple copies of the same helper function through compiled terms (including
 RHS terms compiled by '##' and '##~').

 @since 3.0.2
-}
compile' ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  CompiledTerm a
compile' t = CompiledTerm $ mustCompileD t

{- | Convert a 'CompiledTerm' to a 'Script'.

 @since 3.0.2
-}
toDebuggableScript ::
  forall (a :: S -> Type).
  CompiledTerm a ->
  DebuggableScript
toDebuggableScript (CompiledTerm dscript) = dscript

{- | Apply a 'CompiledTerm' to a closed Plutarch 'Term'.

 Evaluates the argument before applying. You want this for benchmarking the
 compiled function. Helps to avoid tainting the measurement by input
 conversions.

 @since 3.0.2
-}
applyCompiledTerm ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
applyCompiledTerm (CompiledTerm sf) a =
  CompiledTerm $ applyDebuggableScript sf (mustEvalD $ mustCompileD a)

{- | Apply a 'CompiledTerm' to a closed Plutarch 'Term'.

 Does NOT evaluate the argument before applying. Using this seems to save very
 little overhead, not worth it for efficiency. Only use it to make argument
 evaluation count for benchmarking.

 @since 3.0.2
-}
applyCompiledTerm' ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
applyCompiledTerm' (CompiledTerm sf) a =
  CompiledTerm $ applyDebuggableScript sf (mustCompileD a)

{- | Apply a 'CompiledTerm' to a 'CompiledTerm'.

 Evaluates the argument before applying. You want this for benchmarking the
 compiled function. Helps to avoid tainting the measurement by input
 conversions.

 @since 3.0.2
-}
applyCompiledTerm2 ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
applyCompiledTerm2 (CompiledTerm sf) (CompiledTerm sa) =
  CompiledTerm $ applyDebuggableScript sf (mustEvalD sa)

{- | Apply a 'CompiledTerm' to a 'CompiledTerm'.

 Does NOT evaluate the argument before applying. Using this seems to save very
 little overhead, not worth it for efficiency. Only use it to make argument
 evaluation count for benchmarking.

 @since 3.0.2
-}
applyCompiledTerm2' ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
applyCompiledTerm2' (CompiledTerm sf) (CompiledTerm sa) =
  CompiledTerm $ applyDebuggableScript sf sa

{- | Alias for 'applyCompiledTerm'.

 @since 3.0.2
-}
(##) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
(##) = applyCompiledTerm

infixl 8 ##

{- | Alias for 'applyCompiledTerm\''.

 @since 3.0.2
-}
(##~) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
(##~) = applyCompiledTerm'

infixl 8 ##~

{- | Alias for 'applyCompiledTerm2'.

 @since 3.0.2
-}
(###) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
(###) = applyCompiledTerm2

infixl 7 ###

{- | Alias for 'applyCompiledTerm2\''.

 @since 3.0.2
-}
(###~) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
(###~) = applyCompiledTerm2'

infixl 7 ###~

scriptToTerm :: forall (a :: S -> Type) (s :: S). Script -> Term s a
scriptToTerm (Script prog) =
  Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm prog) []

-- | Make a human-readable message from a 'LiftError'.
liftErrorMsg :: LiftError -> String
-- There is no Show instance for LiftError:
-- We would need to get Show for 'KnownTypeError' into 'PlutusCore.Builtin',
-- then Show for 'LiftError' into Plutarch.
-- Though seeing the data constructors only would not be very informative
-- anyway.
liftErrorMsg = \case
  LiftError_FromRepr -> "pconstantFromRepr returned 'Nothing'"
  LiftError_KnownTypeError e ->
    case e of
      KnownTypeUnliftingError unliftErr ->
        "incorrect type: " <> show unliftErr
      KnownTypeEvaluationFailure ->
        "absurd evaluation failure"
  LiftError_EvalError e -> "erring term: " <> show e
  LiftError_CompilationError msg -> "compilation failed: " <> Text.unpack msg

{- | Convert a 'CompiledTerm' to the associated Haskell value. Fail otherwise.

 This will fully evaluate the compiled term, and convert the resulting value.

 @since 3.0.2
-}
pliftCompiled' ::
  forall (p :: S -> Type).
  PUnsafeLiftDecl p =>
  CompiledTerm p ->
  Either (LiftError, [Text]) (PLifted p)
pliftCompiled' ct =
  case res of
    Left evalError -> Left (LiftError_EvalError evalError, traces)
    Right evaluatedScript ->
      case plift'
        (Config {tracingMode = DetTracing})
        (scriptToTerm @p evaluatedScript) of
        Right lifted -> Right lifted
        Left (LiftError_EvalError evalError) ->
          error . unlines $
            [ "Lifting EVALUATED compiled term resulted in "
                <> "LiftError_EvalError!"
            , show evalError
            ]
        Left (LiftError_CompilationError compilationMsg) ->
          error . unlines $
            [ "Lifting evaluated COMPILED term resulted in "
                <> "LiftError_CompilationError!"
            , Text.unpack compilationMsg
            ]
        Left liftError -> handleOtherLiftError liftError
  where
    (res, _, traces) = finalEvalDebuggableScript ct.debuggableScript
    (res', _, traces') = evalScript ct.debuggableScript.debugScript
    handleOtherLiftError liftError =
      case res' of
        Left evalError ->
          error . unlines $
            [ "Script succeeded, but corresponding debug Script failed!"
            , show evalError
            , "Debug Script traces:"
            , Text.unpack (Text.unlines traces')
            , "The debug Script was tried because of a LiftError."
            , "The original LiftError of the succeeded Script:"
            , liftErrorMsg liftError
            ]
        Right evaluatedDebugScript ->
          case plift'
            (Config {tracingMode = DetTracing})
            (scriptToTerm @p evaluatedDebugScript) of
            Right _ ->
              error . unlines $
                [ "Lifting evaluated compiled term resulted in a "
                    <> "LiftError, but lifting the debug version "
                    <> "succeeded!"
                , "The LiftError:"
                , liftErrorMsg liftError
                ]
            Left liftError' ->
              if liftError' == liftError
                then Left (liftError, traces')
                else
                  error . unlines $
                    [ "Lifting Script and corresponding debug "
                        <> "Script resulted in different "
                        <> "LiftErrors!"
                    , "Original LiftError:"
                    , liftErrorMsg liftError
                    , "Debug Script LiftError:"
                    , liftErrorMsg liftError'
                    ]

{- | Like `pliftCompiled'` but throws on failure.

 @since 3.0.2
-}
pliftCompiled ::
  forall (p :: S -> Type).
  (HasCallStack, PLift p) =>
  CompiledTerm p ->
  PLifted p
pliftCompiled ct =
  case pliftCompiled' ct of
    Left (liftError, traces) ->
      error $
        unlines
          [ "Lifting compiled term failed:"
          , liftErrorMsg liftError
          , "Traces:"
          , Text.unpack (Text.unlines traces)
          ]
    Right x -> x
