{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.TermCont (
    module Extra,
    pguardWithC,
) where

import "plutarch-extra" Plutarch.Extra.TermCont as Extra (
    pguardC,
    pguardC',
    pletC,
    pletFieldsC,
    pmatchC,
    ptraceC,
    ptryFromC,
 )
import Plutarch.Prelude

{- | 'pguardC' but with type threading for better traces.

  == Example

  Typical 'pguard' usage:

  @'pguardC' "foo should be even" ('Plutarch.Extra.Numeric.peven' # foo)@

  This is great, but won't tell us what @foo@ _is_, when it isn't even.
  Thankfully, we can augment this using 'pguardWithC':

  @'pguardWithC' (\x -> "foo should be even. it was " <> pshow x) ('peven' #) foo@

  @since 1.1.0
-}
pguardWithC ::
    forall (r :: PType) (pt :: PType) (s :: S).
    -- | Function to print in case of guard failure.
    --   Only gets included in binary when compiling with @development@ flag.
    (Term s pt -> Term s PString) ->
    -- | Function to check for validity of element. Always gets included in script binary.
    (Term s pt -> Term s PBool) ->
    Term s pt ->
    TermCont @r s ()
pguardWithC tracer checker object =
    pguardC (tracer object) (checker object)
