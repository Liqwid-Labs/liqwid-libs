module Plutarch.Extra.StateThread (
  withStateThread,
  pwithStateThread,
) where

import Plutarch.Api.V1 (PCurrencySymbol, PValue)
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PMintingPolicy,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxOutRef,
 )
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.List (ptryFromSingleton)
import Plutarch.Extra.Maybe (pfromJust)

{- | Adds a state thread to a minting policy.
 Parameterized at the Haskell level.

 @since 3.19.0
-}
withStateThread ::
  forall (s :: S).
  -- | Minting policy to wrap
  Term s PMintingPolicy ->
  -- | Initial spend
  Term s PTxOutRef ->
  Term s PMintingPolicy
withStateThread mp ref = plam $ \red ctx -> pletAll ctx $ \ctx' ->
  pletFields @'["inputs", "mint"] (getField @"txInfo" ctx') $ \txInfo ->
    pmatch (getField @"purpose" ctx') $ \case
      PMinting thisPolicy ->
        pif
          (uniqueStateTokenMint (pfield @"_0" # thisPolicy) . getField @"mint" $ txInfo)
          ( pif
              (pany # (hasUniqueInput # ref) # getField @"inputs" txInfo)
              (mp # red # ctx)
              (ptraceError "stateThread: Unique input not found")
          )
          (ptraceError "stateThread: Not minting a unique state token")
      _ -> ptraceError "stateThread: Not a minting script purpose"

{- | Adds a state thread to a minting policy.
 Parameterized at the Plutarch level

 @since 3.19.0
-}
pwithStateThread ::
  forall (s :: S).
  Term s (PMintingPolicy :--> PTxOutRef :--> PMintingPolicy)
pwithStateThread = plam withStateThread

-- Helpers

uniqueStateTokenMint ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PValue keys amounts) ->
  Term s PBool
uniqueStateTokenMint thisPolicy mint =
  let singleEmptyToken = ppairDataBuiltin # pdata (pconstant "") # pdata 1
   in ptryFromSingleton
        # pto (pfromJust #$ plookup # thisPolicy # pto mint)
        #== singleEmptyToken

hasUniqueInput ::
  forall (s :: S).
  Term s (PTxOutRef :--> PTxInInfo :--> PBool)
hasUniqueInput =
  plam $ \ref txInInfo -> ref #== (pfield @"outRef" # txInInfo)
