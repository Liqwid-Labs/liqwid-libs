module Plutarch.Extra.StateThread (stateThread) where

import Plutarch.Api.V1 (PCurrencySymbol, PValue)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
 )
import Plutarch.Extra.Field (pletAll)
import PlutusLedgerApi.V2 (TxOutRef)

-- | @since 3.9.2
stateThread ::
  forall (s :: S).
  TxOutRef ->
  Term s (PData :--> PScriptContext :--> POpaque)
stateThread ref = plam $ \_ ctx -> pletAll ctx $ \ctx' ->
  pletFields @'["inputs", "mint"] (getField @"txInfo" ctx') $ \txInfo ->
    pmatch (getField @"purpose" ctx') $ \case
      PMinting thisPolicy ->
        pif
          (validMint (pfield @"_0" # thisPolicy) . getField @"mint" $ txInfo)
          ( pif
              (pany # hasUniqueInput ref # getField @"inputs" txInfo)
              (popaque ctx)
              (ptraceError "stateThread: Input not unique")
          )
          (ptraceError "stateThread: Not a valid mint")
      _ -> ptraceError "stateThread: Not a minting script purpose"

-- Helpers

validMint ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PValue keys amounts) ->
  Term s PBool
validMint thisPolicy mint =
  pvalueOf # mint # thisPolicy # pconstant "" #== pconstant 1

hasUniqueInput ::
  forall (s :: S).
  TxOutRef ->
  Term s (PTxInInfo :--> PBool)
hasUniqueInput ref =
  plam $ \txInInfo -> pconstantData ref #== (pfield @"outRef" # txInInfo)
