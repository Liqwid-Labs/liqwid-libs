{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Benchmark.Precompile (
  compile',
  toScript,
  apply,
  exploitUnsafety,
) where

import Data.Text qualified as Text
import Debug.Trace (traceShow)
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

newtype CompiledTerm (s :: S) (a :: S -> Type) = CompiledTerm Script

compile' :: forall (a :: S -> Type) (s :: S). ClosedTerm a -> CompiledTerm s a
compile' t = CompiledTerm $ compile t

toScript :: forall (a :: S -> Type) (s :: S). CompiledTerm s a -> Script
toScript (CompiledTerm script) = script

applyCompiledTerm ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  CompiledTerm s (a :--> b) ->
  Term s a ->
  CompiledTerm s b
applyCompiledTerm (CompiledTerm sf) a =
  CompiledTerm $ applyScript sf (eval $ compile $ unsafeCoerce a)

class ApplyN (b :: Type) (pb :: S -> Type) s | s pb -> b, b -> pb s where
  apply ::
    forall (pa :: S -> Type).
    CompiledTerm s (pa :--> pb) ->
    Term s pa ->
    b

instance (ApplyN c pc s) => ApplyN (Term s px -> c) (px :--> pc) s where
  apply ::
    forall (pa :: S -> Type).
    CompiledTerm s (pa :--> px :--> pc) ->
    Term s pa ->
    Term s px ->
    c
  apply f x = apply (applyCompiledTerm f x)

instance {-# OVERLAPPABLE #-} (b ~ CompiledTerm s pb) => ApplyN b pb s where
  apply = applyCompiledTerm

{- | This subverts type safety, making use of the unsafeCoerce in 'applyCompiledTerm'.

 It only seems to lead to the error "Cannot evaluate an open term", nothing
 severe seems to happen.
-}
exploitUnsafety :: forall (s :: S). Term s PUnit
exploitUnsafety =
  plet
    (pconstant 23 :: Term s PInteger)
    ( \x ->
        traceShow
          ( evalScript $
              toScript $
                applyCompiledTerm (compile' (plam pshow)) x
          )
          (pconstant ())
    )
