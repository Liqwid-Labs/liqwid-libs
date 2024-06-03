{-# LANGUAGE RankNTypes #-}

{- | Pre-compiling Plutarch functions and applying them.

 Speeds up benchmarking and testing.
-}
module Plutarch.Extra.Precompile (
  -- Exporting the data constructor on purpose, since users might want to
  -- deserialize compiled terms.  If someone wants to subvert type safety using
  -- Scripts, they can do that regardless of this export.
  CompiledTerm (..),
  debuggableScript,
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
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Optics.Getter (view)
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.DebuggableScript (
  DebuggableScript,
  applyDebuggableArg,
  finalEvalDebuggableScript,
  mustCompileD,
  mustEvalD,
 )
import Plutarch.Internal (
  Config (Tracing),
  LogLevel (LogInfo),
  RawTerm (RCompiled),
  Term (Term),
  TermResult (TermResult),
  TracingMode (DetTracing),
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
import Plutarch.Script (Script (Script))
import PlutusCore.Builtin (KnownTypeError (KnownTypeEvaluationFailure, KnownTypeUnliftingError))
import UntypedPlutusCore qualified as UPLC

{- | Type-safe wrapper for compiled Plutarch functions.

 @since 3.8.0
-}
newtype CompiledTerm (a :: S -> Type) = CompiledTerm DebuggableScript

-- | @since 3.8.0
debuggableScript ::
  forall (a :: S -> Type).
  CompiledTerm a ->
  DebuggableScript
debuggableScript (CompiledTerm x) = x

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
  CompiledTerm $ applyDebuggableArg sf (mustEvalD $ mustCompileD a)

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
  CompiledTerm $ applyDebuggableArg sf (mustCompileD a)

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
  CompiledTerm $ applyDebuggableArg sf (mustEvalD sa)

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
  CompiledTerm $ applyDebuggableArg sf sa

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
  (PUnsafeLiftDecl p) =>
  CompiledTerm p ->
  Either (LiftError, [Text]) (PLifted p)
pliftCompiled' ct =
  case res of
    Left evalError -> Left (LiftError_EvalError evalError, traces)
    Right evaluatedScript ->
      case plift'
        (Tracing LogInfo DetTracing)
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
    (res, _, traces) = finalEvalDebuggableScript . debuggableScript $ ct
    (res', _, traces') = evalScript . view #debugScript . debuggableScript $ ct
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
            (Tracing LogInfo DetTracing)
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
