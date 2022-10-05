module Plutarch.Extra.StateThread (
  withStateThread,
) where

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

{- | Adds a state thread to a minting policy.

 @since 3.9.2
-}
withStateThread ::
  forall (s :: S).
  -- | Initial spend
  TxOutRef ->
  -- | Minting policy to wrap
  Term s (PData :--> PScriptContext :--> POpaque) ->
  Term s (PData :--> PScriptContext :--> POpaque)
withStateThread ref mp = plam $ \red ctx -> pletAll ctx $ \ctx' ->
  pletFields @'["inputs", "mint"] (getField @"txInfo" ctx') $ \txInfo ->
    pmatch (getField @"purpose" ctx') $ \case
      PMinting thisPolicy ->
        pif
          (uniqueStateTokenMint (pfield @"_0" # thisPolicy) . getField @"mint" $ txInfo)
          ( pif
              (pany # hasUniqueInput ref # getField @"inputs" txInfo)
              (mp # red # ctx)
              (ptraceError "stateThread: Unique input not found")
          )
          (ptraceError "stateThread: Not minting a unique state token")
      _ -> ptraceError "stateThread: Not a minting script purpose"

-- Helpers

uniqueStateTokenMint ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PValue keys amounts) ->
  Term s PBool
uniqueStateTokenMint thisPolicy mint =
  pvalueOf # mint # thisPolicy # pconstant "" #== pconstant 1

hasUniqueInput ::
  forall (s :: S).
  TxOutRef ->
  Term s (PTxInInfo :--> PBool)
hasUniqueInput ref =
  plam $ \txInInfo -> pconstantData ref #== (pfield @"outRef" # txInInfo)
