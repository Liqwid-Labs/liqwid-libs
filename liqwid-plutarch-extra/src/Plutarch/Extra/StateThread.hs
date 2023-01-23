{-# LANGUAGE Rank2Types #-}

module Plutarch.Extra.StateThread (
  withStateThread,
  pwithStateThread,
  withStateThreadMulti,
  pwithStateThreadMulti,
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
withStateThread = withStateThreadGeneric uniqueStateTokenMint

{- | Adds a state thread to a minting policy.
 Parameterized at the Plutarch level

 @since 3.19.0
-}
pwithStateThread ::
  forall (s :: S).
  Term s (PMintingPolicy :--> PTxOutRef :--> PMintingPolicy)
pwithStateThread = plam withStateThread

{- | Adds a state thread to a minting policy
 allowing more than one state thread token to be minted
 Parameterized at the Haskell level.

 @since 3.21.0
-}
withStateThreadMulti ::
  forall (s :: S).
  -- | Minting policy to wrap
  Term s PMintingPolicy ->
  -- | Initial spend
  Term s PTxOutRef ->
  Term s PMintingPolicy
withStateThreadMulti = withStateThreadGeneric uniqueStateTokensMint

{- | Adds a state thread to a minting policy
 allowing more than one state thread token to be minted
 Parameterized at the Plutarch level.

 @since 3.21.0
-}
pwithStateThreadMulti ::
  forall (s :: S).
  Term s (PMintingPolicy :--> PTxOutRef :--> PMintingPolicy)
pwithStateThreadMulti = plam withStateThreadMulti

-- Helpers

{- | Adds a state thread to a minting policy.
 Parameterized at the Haskell level,
 with parametrized check on minted assets.

 @since 3.21.0
-}
withStateThreadGeneric ::
  forall (s :: S).
  ( forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s PCurrencySymbol ->
    Term s (PValue keys amounts) ->
    Term s PBool
  ) ->
  -- | Minting policy to wrap
  Term s PMintingPolicy ->
  -- | Initial spend
  Term s PTxOutRef ->
  Term s PMintingPolicy
withStateThreadGeneric checkMint mp ref = plam $ \red ctx -> pletAll ctx $ \ctx' ->
  pletFields @'["inputs", "mint"] (getField @"txInfo" ctx') $ \txInfo ->
    pmatch (getField @"purpose" ctx') $ \case
      PMinting thisPolicy ->
        pif
          (checkMint (pfield @"_0" # thisPolicy) . getField @"mint" $ txInfo)
          ( pif
              (pany # (hasUniqueInput # ref) # getField @"inputs" txInfo)
              (mp # red # ctx)
              (ptraceError "stateThread: Unique input not found")
          )
          (ptraceError "stateThread: Not minting a unique state token")
      _ -> ptraceError "stateThread: Not a minting script purpose"

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

uniqueStateTokensMint ::
  forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
  Term s PCurrencySymbol ->
  Term s (PValue keys amounts) ->
  Term s PBool
uniqueStateTokensMint thisPolicy mint =
  0 #< pfromData (psndBuiltin #$ ptryFromSingleton # pto (pfromJust #$ plookup # thisPolicy # pto mint))

hasUniqueInput ::
  forall (s :: S).
  Term s (PTxOutRef :--> PTxInInfo :--> PBool)
hasUniqueInput =
  plam $ \ref txInInfo -> ref #== (pfield @"outRef" # txInInfo)
