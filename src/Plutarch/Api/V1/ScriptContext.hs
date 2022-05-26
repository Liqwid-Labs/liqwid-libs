{-# LANGUAGE TypeApplications #-}

module Plutarch.Api.V1.ScriptContext (
    pownTxOutRef,
    pownTxInfo,
    pownValue,
    pownMintValue,
    pownInput,
) where

import Plutarch (S, Term, phoistAcyclic, plam, unTermCont, (#), (:-->))
import Plutarch.Api.V1 (
    PScriptContext,
    PScriptPurpose (PSpending),
    PTxInInfo,
    PTxInfo,
    PTxOutRef,
    PValue,
 )
import Plutarch.Bool (PBool, (#==))
import Plutarch.Builtin (PAsData, pfromData)
import Plutarch.DataRepr (pfield)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.List (pfind)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Trace (ptraceError)

pownTxOutRef ::
    forall (s :: S).
    Term s (PScriptContext :--> PTxOutRef)
pownTxOutRef = phoistAcyclic $
    plam $ \sc -> unTermCont $ do
        PSpending t <- pmatchC (pfield @"purpose" # sc)
        pure $ pfield @"_0" # t

pownTxInfo ::
    forall (s :: S).
    Term s (PScriptContext :--> PTxInfo)
pownTxInfo = phoistAcyclic $ plam $ \sc -> pfield @"txInfo" # sc

pownValue ::
    forall (s :: S).
    Term s (PScriptContext :--> PValue)
pownValue = phoistAcyclic $
    plam $ \sc -> unTermCont $ do
        input <- pletC (pownInput # sc)
        pure $ pfield @"value" # (pfield @"resolved" # input)

pownMintValue ::
    forall (s :: S).
    Term s (PScriptContext :--> PValue)
pownMintValue = phoistAcyclic $ plam $ \sc -> pfield @"mint" # (pownTxInfo # sc)

pownInput ::
    forall (s :: S).
    Term s (PScriptContext :--> PTxInInfo)
pownInput = phoistAcyclic $
    plam $ \sc -> unTermCont $ do
        txInfo <- pletC (pownTxInfo # sc)
        txOutRef <- pletC (pownTxOutRef # sc)
        txInInfos <- pletC (pfromData $ pfield @"inputs" # txInfo)
        res <- pmatchC (pfind # (go # txOutRef) # txInInfos)
        pure $ case res of
            PNothing -> ptraceError "pownInput: Could not find my own input"
            PJust res' -> pfromData res'
  where
    go ::
        forall (s' :: S).
        Term s' (PTxOutRef :--> PAsData PTxInInfo :--> PBool)
    go = phoistAcyclic $
        plam $ \tgt t -> unTermCont $ do
            x <- pletC (pfield @"outRef" # pfromData t)
            pure $ tgt #== x
