{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.TermCont (
    passertC,
    module Extra,
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

import Plutarch (
    S,
    Term,
 )
import Plutarch.Bool (PBool, pif)
import Plutarch.String (PString)
import Plutarch.TermCont (TermCont, tcont)
import Plutarch.Trace (ptraceError)

{- | Assert a particular 'PBool', trace if false.

    @since 1.0.0
-}
passertC ::
    forall r (s :: S).
    Term s PString ->
    Term s PBool ->
    TermCont @r s ()
passertC errorMessage check = tcont $ \k ->
    pif check (k ()) (ptraceError errorMessage)
