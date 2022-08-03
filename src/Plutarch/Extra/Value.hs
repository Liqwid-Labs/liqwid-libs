{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Extra.Value (
    psingletonValue,
    passetClassValue,
    pvalueOf,
    padaOf,
    psymbolValueOf,
    passetClassValueOf',
    pgeqByClass,
    pgeqBySymbol,
    pgeqByClass',
    type AddGuarantees,
    phasOnlyOneTokenOfCurrencySymbol,
) where

import Plutarch (
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    pto,
    unTermCont,
    (#),
    (#$),
    type (:-->),
 )
import Plutarch.Api.V2 (
    AmountGuarantees (..),
    KeyGuarantees,




 )
import Plutarch.Api.V1 (
  PValue (PValue),
  PCurrencySymbol,
  PTokenName,
  PMap (PMap),
  )
import Plutarch.Extra.AssetClass (PAssetClass)
import Plutarch.Bool (PBool, (#&&), (#<=), (#==))
import Plutarch.Builtin (pdata, pfromData, ppairDataBuiltin, psndBuiltin)
import Plutarch.DataRepr (pfield)
import qualified Plutarch.Extra.List
import Plutarch.Extra.Map (plookup)
import Plutarch.Extra.Maybe (pexpectJustC)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.List (pfoldr, plength, psingleton)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import PlutusLedgerApi.V1.Value (AssetClass (..))

{- | Create a `PValue` that only contains specific amount tokens of the given symbol and name.

   @since 1.0.0
-}
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

{- | Create a `PValue` that only contains specific amount tokens of the given assetclass.

   @since 1.0.0
-}
passetClassValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PAssetClass :--> PInteger :--> PValue keys amounts)
passetClassValue = phoistAcyclic $
    plam $ \ac i -> unTermCont $ do
        cs <- pletC (pfield @"currencySymbol" # ac)
        tn <- pletC (pfield @"tokenName" # ac)
        pure $ psingletonValue # pfromData cs # pfromData tn # i

{- | Get the amount of tokens which have the given symbol and name from a `PValue`.

   @since 1.0.0
-}
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

{- | Get the amount of ada of a `PValue`.

   @since 1.0.0
-}
padaOf ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PValue keys amounts :--> PInteger)
padaOf = phoistAcyclic $ plam $ \v -> pvalueOf # v # pconstant "" # pconstant ""

{- | Get the sum of all values belonging to a particular CurrencySymbol.

   @since 1.1.0
-}
psymbolValueOf ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PValue keys amounts :--> PInteger)
psymbolValueOf =
    phoistAcyclic $
        plam $ \sym value'' -> unTermCont $ do
            PValue value' <- pmatchC value''
            PMap value <- pmatchC value'
            m' <- pexpectJustC 0 (Plutarch.Extra.List.plookup # pdata sym # value)
            PMap m <- pmatchC (pfromData m')
            pure $ pfoldr # plam (\x v -> pfromData (psndBuiltin # x) + v) # 0 # m

{- | Extract amount from PValue belonging to a Haskell-level AssetClass.

   @since 1.1.0
-}
passetClassValueOf' ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    AssetClass ->
    Term s (PValue keys amounts :--> PInteger)
passetClassValueOf' (AssetClass (sym, token)) =
    phoistAcyclic $ plam $ \value -> pvalueOf # value # pconstant sym # pconstant token

{- | Return '>=' on two values comparing by only a particular AssetClass.

   @since 1.1.0
-}
pgeqByClass ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PTokenName :--> PValue keys amounts :--> PValue keys amounts :--> PBool)
pgeqByClass =
    phoistAcyclic $
        plam $ \cs tn a b ->
            pvalueOf # b # cs # tn #<= pvalueOf # a # cs # tn

{- | Return '>=' on two values comparing by only a particular CurrencySymbol.

   @since 1.1.0
-}
pgeqBySymbol ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PValue keys amounts :--> PValue keys amounts :--> PBool)
pgeqBySymbol =
    phoistAcyclic $
        plam $ \cs a b ->
            psymbolValueOf # cs # b #<= psymbolValueOf # cs # a

{- | Return '>=' on two values comparing by only a particular Haskell-level AssetClass.

   @since 1.1.0
-}
pgeqByClass' ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    AssetClass ->
    Term s (PValue keys amounts :--> PValue keys amounts :--> PBool)
pgeqByClass' ac =
    phoistAcyclic $
        plam $ \a b ->
            passetClassValueOf' ac # b #<= passetClassValueOf' ac # a

{- | Compute the guarantees known after adding two values.

   @since 1.1.0
-}
type family AddGuarantees (a :: AmountGuarantees) (b :: AmountGuarantees) where
    AddGuarantees 'Positive 'Positive = 'Positive
    AddGuarantees _ _ = 'NoGuarantees

{- | The entire value only contains one token of the given currency symbol.
     @since 1.3.0
-}
phasOnlyOneTokenOfCurrencySymbol ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PValue keys amounts :--> PBool)
phasOnlyOneTokenOfCurrencySymbol = phoistAcyclic $
    plam $ \cs vs ->
        psymbolValueOf # cs # vs #== 1
            #&& (plength #$ pto $ pto $ pto vs) #== 1
