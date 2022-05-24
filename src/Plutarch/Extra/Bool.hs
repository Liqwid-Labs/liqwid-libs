module Plutarch.Extra.Bool (
    pcompare,
) where

import Data.Kind (Type)
import Plutarch (S, Term)
import Plutarch.Bool (POrd ((#<)), pif, (#==))

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
