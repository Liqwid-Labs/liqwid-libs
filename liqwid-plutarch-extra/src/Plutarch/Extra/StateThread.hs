{-# LANGUAGE Rank2Types #-}

module Plutarch.Extra.StateThread (
  withStateThread,
  pwithStateThread,
  withStateThreadMulti,
  pwithStateThreadMulti,
) where

import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.List (ptryFromSingleton)
import Plutarch.Extra.Maybe (pfromJust)
import Plutarch.LedgerApi (
  AmountGuarantees,
  KeyGuarantees,
  PCurrencySymbol,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxOutRef,
  PValue,
 )
import Plutarch.LedgerApi.AssocMap (plookup)

{- | Adds a state thread to a minting policy.
 Parameterized at the Haskell level.

 @since 3.19.0
-}
withStateThread ::
  forall (s :: S).
  -- | Minting policy to wrap
  Term s (PData :--> PScriptContext :--> POpaque) ->
  -- | Initial spend
  Term s PTxOutRef ->
  Term s (PData :--> PScriptContext :--> POpaque)
withStateThread = withStateThreadGeneric uniqueStateTokenMint

{- | Adds a state thread to a minting policy.
 Parameterized at the Plutarch level

 @since 3.19.0
-}
pwithStateThread ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PScriptContext :--> POpaque)
        :--> PTxOutRef
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pwithStateThread = plam withStateThread

{- | Adds a state thread to a minting policy
 allowing more than one state thread token to be minted
 Parameterized at the Haskell level.

 @since 3.21.0
-}
withStateThreadMulti ::
  forall (s :: S).
  -- | Minting policy to wrap
  Term s (PData :--> PScriptContext :--> POpaque) ->
  -- | Initial spend
  Term s PTxOutRef ->
  Term s (PData :--> PScriptContext :--> POpaque)
withStateThreadMulti = withStateThreadGeneric uniqueStateTokensMint

{- | Adds a state thread to a minting policy
 allowing more than one state thread token to be minted
 Parameterized at the Plutarch level.

 @since 3.21.0
-}
pwithStateThreadMulti ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PScriptContext :--> POpaque)
        :--> PTxOutRef
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
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
  Term s (PData :--> PScriptContext :--> POpaque) ->
  -- | Initial spend
  Term s PTxOutRef ->
  Term s (PData :--> PScriptContext :--> POpaque)
withStateThreadGeneric checkMint mp ref = plam $ \red ctx -> pletAll ctx $ \ctx' ->
  pletFields @'["inputs", "mint"] (getField @"txInfo" ctx') $ \txInfo ->
    pmatch (getField @"purpose" ctx') $ \case
      PMinting thisPolicy ->
        pif
          (checkMint (pfield @"_0" # thisPolicy) . getField @"mint" $ txInfo)
          ( pif
              (pany # (hasUniqueInput # ref) # getField @"inputs" txInfo)
              (mp # red # ctx)
              (ptraceInfoError "stateThread: Unique input not found")
          )
          (ptraceInfoError "stateThread: Not minting a unique state token")
      _ -> ptraceInfoError "stateThread: Not a minting script purpose"

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
