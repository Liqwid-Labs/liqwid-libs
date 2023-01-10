module Plutarch.Extra.Bool (
  pcompare,
  pcond,
  passert,
) where

--------------------------------------------------------------------------------

import Data.Monoid (Endo (Endo, appEndo))

--------------------------------------------------------------------------------

{- | Perform a \'three-way\' comparison on two 'Term's, then return a result
 based on the outcome.

 @since 1.0.0
-}
pcompare ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (POrd a) =>
  -- | First 'Term'
  Term s a ->
  -- | Second 'Term'
  Term s a ->
  -- | Result if first 'Term' is smaller than second
  Term s b ->
  -- | Result if first 'Term' is equal to second
  Term s b ->
  -- | Result if first 'Term' is greater than second
  Term s b ->
  -- | Final outcome based on test
  Term s b
pcompare t1 t2 ifLT ifEQ ifGT =
  pif (t1 #< t2) ifLT (pif (t1 #== t2) ifEQ ifGT)

{- |
  Lisp-like cond:
    chain together if conditions and the final else case

  @since 3.9.1
-}
pcond :: forall (s :: S) (a :: S -> Type). [Term s a -> Term s a] -> Term s a -> Term s a
pcond = appEndo . foldMap Endo

{- | If the condition evaluated to true, return the third argument. Otherwise error
     out with the error message.

     @since 3.14.1
-}
passert ::
  forall (a :: PType) (s :: S).
  -- | The error message.
  Term s PString ->
  -- | The condition.
  Term s PBool ->
  -- | The result.
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceError msg
