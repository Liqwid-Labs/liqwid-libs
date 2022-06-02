{-# LANGUAGE TypeApplications #-}

module Plutarch.Api.V1.Value (
    psingletonValue,
    passetClassValue,
    pvalueOf,
    padaOf,
) where

import Plutarch (
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Api.V1 (
    AmountGuarantees,
    KeyGuarantees,
    PCurrencySymbol,
    PMap (PMap),
    PTokenName,
    PValue (PValue),
 )
import Plutarch.Api.V1.AssetClass (PAssetClass)
import Plutarch.Builtin (pdata, pfromData, ppairDataBuiltin)
import Plutarch.DataRepr (pfield)
import Plutarch.Extra.Map (plookup)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.List (psingleton)
import Plutarch.Maybe (PMaybe (PJust, PNothing))

psingletonValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PValue keys amounts)
psingletonValue = phoistAcyclic $
    plam $ \cs tn i -> unTermCont $ do
        innerPair <- pletC (ppairDataBuiltin # pdata tn # pdata i)
        inner <- pletC (pcon . PMap $ psingleton # innerPair)
        outerPair <- pletC (ppairDataBuiltin # pdata cs # pdata inner)
        outer <- pletC (pcon . PMap $ psingleton # outerPair)
        pure . pcon . PValue $ outer

-- | @since 1.0.0
passetClassValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PAssetClass :--> PInteger :--> PValue keys amounts)
passetClassValue = phoistAcyclic $
    plam $ \ac i -> unTermCont $ do
        cs <- pletC (pfield @"currencySymbol" # ac)
        tn <- pletC (pfield @"tokenName" # ac)
        pure $ psingletonValue # pfromData cs # pfromData tn # i

-- | @since 1.0.0
pvalueOf ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PValue keys amounts :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
    plam $ \val cs tn -> unTermCont $ do
        PValue m <- pmatchC val
        innerMay <- pmatchC (plookup # cs # m)
        case innerMay of
            PNothing -> pure 0
            PJust inner -> do
                resMay <- pmatchC (plookup # tn # inner)
                pure $ case resMay of
                    PNothing -> 0
                    PJust res -> res

-- | @since 1.0.0
padaOf ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PValue keys amounts :--> PInteger)
padaOf = phoistAcyclic $ plam $ \v -> pvalueOf # v # pconstant "" # pconstant ""
