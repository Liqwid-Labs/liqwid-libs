{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Benchmark.Precompile2 (compile', toScript, apply) where

import Data.Text qualified as Text
import Plutarch (compile)
import Plutarch.Evaluate (evalScript)
import PlutusLedgerApi.V1.Scripts (Script (Script))
import Unsafe.Coerce (unsafeCoerce)
import UntypedPlutusCore (Program (_progAnn, _progVer))
import UntypedPlutusCore.Core.Type (Program (Program, _progTerm))
import UntypedPlutusCore.Core.Type qualified as UplcType

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

eval :: Script -> Script
eval s =
  case res of
    Left e -> error $ Text.unpack (Text.unlines $ Text.pack (show e) : traces)
    Right sr -> sr
  where
    (res, _, traces) = evalScript s

-- differences to the other variant:

newtype CompiledTerm (a :: S -> Type) = CompiledTerm Script

compile' :: forall (a :: S -> Type). ClosedTerm a -> CompiledTerm a
compile' t = CompiledTerm $ compile t

toScript :: forall (a :: S -> Type). CompiledTerm a -> Script
toScript (CompiledTerm script) = script

applyCompiledTerm ::
  forall (a :: S -> Type) (b :: S -> Type).
  CompiledTerm (a :--> b) ->
  ClosedTerm a ->
  CompiledTerm b
applyCompiledTerm (CompiledTerm sf) a =
  CompiledTerm $ applyScript sf (eval $ compile a)

class ApplyN (b :: Type) (pb :: S -> Type) s | s pb -> b where
  apply :: forall (pa :: S -> Type). CompiledTerm (pa :--> pb) -> Term s pa -> b

instance {-# OVERLAPPABLE #-} (b ~ CompiledTerm pb) => ApplyN b pb s where
  apply ::
    forall (pa :: S -> Type).
    CompiledTerm (pa :--> pb) ->
    Term s pa ->
    CompiledTerm pb
  apply f x = applyCompiledTerm f (unsafeCoerce x :: ClosedTerm pa)

instance (ApplyN c pc s) => ApplyN (Term s px -> c) (px :--> pc) s where
  apply ::
    forall (pa :: S -> Type).
    CompiledTerm (pa :--> px :--> pc) ->
    Term s pa ->
    Term s px ->
    c
  apply f x = apply (applyCompiledTerm f (unsafeCoerce x :: ClosedTerm pa))
