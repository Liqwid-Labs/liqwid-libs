{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{- | Pre-compiling Plutarch functions and applying them.

 Speeds up benchmarking and testing.
-}
module Test.Benchmark.Precompile (
  applyScript,
  CompiledTerm,
  compile',
  toScript,
  applyCompiledTerm,
  applyCompiledTerm',
  applyCompiledTerm2,
  applyCompiledTerm2',
  (##),
  (##~),
  (###),
  (###~),
  LiftError (..),
  pliftCompiled',
  pliftCompiled,
) where

import Control.Lens ((^?))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Plutarch (compile)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Lift (PConstantDecl (pconstantFromRepr), PUnsafeLiftDecl (PLifted))
import PlutusCore.Builtin (KnownTypeError, readKnownConstant)
import PlutusCore.Evaluation.Machine.Exception (_UnliftingErrorE)
import PlutusLedgerApi.V1.Scripts (Script (Script, unScript))
import UntypedPlutusCore (Program (Program, _progAnn, _progTerm, _progVer))
import UntypedPlutusCore.Core.Type qualified as UplcType

-- | Apply a function to an argument on the compiled 'Script' level.
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

-- | Evaluate a 'Script' with errors resulting in exceptions.
eval :: Script -> Script
eval s =
  case res of
    Left e -> error $ Text.unpack (Text.unlines $ Text.pack (show e) : traces)
    Right sr -> sr
  where
    (res, _, traces) = evalScript s

-- | Type-safe wrapper for compiled Plutarch functions.
newtype CompiledTerm (a :: S -> Type) = CompiledTerm Script

-- | Compile a closed Plutarch 'Term' to a 'CompiledTerm'.
compile' ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  CompiledTerm a
compile' t = CompiledTerm $ compile t

-- | Convert a 'CompiledTerm' to a 'Script'.
toScript :: forall (a :: S -> Type). CompiledTerm a -> Script
toScript (CompiledTerm script) = script

{- | Apply a 'CompiledTerm' to a closed Plutarch 'Term'.

 Evaluates the argument before applying. You want this for benchmarking the
 compiled function. Helps to avoid tainting the measurement by input
 conversions.
-}
applyCompiledTerm ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
applyCompiledTerm (CompiledTerm sf) a =
  CompiledTerm $ applyScript sf (eval $ compile a)

{- | Apply a 'CompiledTerm' to a closed Plutarch 'Term'.

 Does NOT evaluate the argument before applying. Using this seems to save very
 little overhead, not worth it for efficiency. Only use it to make argument
 evaluation count for benchmarking.
-}
applyCompiledTerm' ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
applyCompiledTerm' (CompiledTerm sf) a =
  CompiledTerm $ applyScript sf (compile a)

{- | Apply a 'CompiledTerm' to a 'CompiledTerm'.

 Evaluates the argument before applying. You want this for benchmarking the
 compiled function. Helps to avoid tainting the measurement by input
 conversions.
-}
applyCompiledTerm2 ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
applyCompiledTerm2 (CompiledTerm sf) (CompiledTerm sa) =
  CompiledTerm $ applyScript sf (eval sa)

{- | Apply a 'CompiledTerm' to a 'CompiledTerm'.

 Does NOT evaluate the argument before applying. Using this seems to save very
 little overhead, not worth it for efficiency. Only use it to make argument
 evaluation count for benchmarking.
-}
applyCompiledTerm2' ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
applyCompiledTerm2' (CompiledTerm sf) (CompiledTerm sa) =
  CompiledTerm $ applyScript sf sa

-- | Alias for 'applyCompiledTerm'.
(##) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
(##) = applyCompiledTerm

infixl 8 ##

-- | Alias for 'applyCompiledTerm\''.
(##~) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
(##~) = applyCompiledTerm'

infixl 8 ##~

-- | Alias for 'applyCompiledTerm2'.
(###) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
(###) = applyCompiledTerm2

infixl 7 ###

-- | Alias for 'applyCompiledTerm2\''.
(###~) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  CompiledTerm a ->
  CompiledTerm b
(###~) = applyCompiledTerm2'

infixl 7 ###~

{- | Error during script evaluation.

 Copied from 'Plutarch.Lift' because the data constructors aren't exported there.
 Also added logs.
-}
data LiftError
  = LiftError_EvalError EvalError [Text]
  | LiftError_KnownTypeError KnownTypeError
  | LiftError_FromRepr
  deriving stock (Eq)

{- | Convert a 'CompiledTerm' to the associated Haskell value. Fail otherwise.
 This will fully evaluate the arbitrary closed expression, and convert the resulting value.

 Copied and adapted from 'Plutarch.Lift'.
 Also added logs.
-}
pliftCompiled' :: forall p. PUnsafeLiftDecl p => CompiledTerm p -> Either LiftError (PLifted p)
pliftCompiled' prog = case evalScript (toScript prog) of
  (Right (unScript -> UplcType.Program _ _ term), _, _) ->
    case readKnownConstant term of
      Right r -> case pconstantFromRepr r of
        Just h -> Right h
        Nothing -> Left LiftError_FromRepr
      Left e -> Left $ LiftError_KnownTypeError e
  (Left e, _, logs) -> Left $ LiftError_EvalError e logs

{- | Like `pliftCompiled'` but throws on failure.

 Copied and adapted from 'Plutarch.Lift' (the one from there doesn't show logs
 and can't work with Scripts).
-}
pliftCompiled :: forall p. (HasCallStack, PLift p) => CompiledTerm p -> PLifted p
pliftCompiled prog = case pliftCompiled' prog of
  Right x -> x
  Left LiftError_FromRepr -> error "plift failed: pconstantFromRepr returned 'Nothing'"
  Left (LiftError_KnownTypeError e) ->
    let unliftErrMaybe = e ^? _UnliftingErrorE
     in error $
          "plift failed: incorrect type: "
            <> maybe "absurd evaluation failure" show unliftErrMaybe
  Left (LiftError_EvalError e logs) ->
    error $
      "plift failed: erring term: "
        <> Text.unpack (Text.unlines $ Text.pack (show e) : logs)
