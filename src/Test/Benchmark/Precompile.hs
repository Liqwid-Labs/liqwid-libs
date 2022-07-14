{- | Pre-compiling Plutarch functions and applying them.

 Speeds up benchmarking and testing.
-}
module Test.Benchmark.Precompile (
  compile',
  toScript,
  applyCompiledTerm,
  (##),
) where

import Data.Text qualified as Text
import Plutarch (compile)
import Plutarch.Evaluate (evalScript)
import PlutusLedgerApi.V1.Scripts (Script (Script))
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

 Evaluates the argument before applying.
-}
applyCompiledTerm ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
applyCompiledTerm (CompiledTerm sf) a =
  CompiledTerm $ applyScript sf (eval $ compile a)

-- | Alias for 'applyCompiledTerm'.
(##) ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  (forall (s :: S). Term s a) ->
  CompiledTerm b
(##) = applyCompiledTerm

infixl 8 ##