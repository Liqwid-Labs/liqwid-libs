{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Extra.AssetClass (
    PAssetClass (..),
    passetClass,
    passetClassValueOf,
    pvalueOf,
) where

import Plutarch.Api.V1 (
    PCurrencySymbol,
    PTokenName,
    PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Api.V2 (
    AmountGuarantees,
    KeyGuarantees,
 )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.TermCont (pletC, pmatchC)

-- | @since 1.0.0
newtype PAssetClass (s :: S)
    = PAssetClass
        ( Term
            s
            ( PDataRecord
                '[ "currencySymbol" ':= PCurrencySymbol
                 , "tokenName" ':= PTokenName
                 ]
            )
        )
    deriving stock
        ( -- | @since 0.1.0
          Generic
        )
    deriving anyclass
        ( -- | @since 0.1.0
          PlutusType
        , -- | @since 0.1.0
          PIsData
        , -- | @since 0.1.0
          PDataFields
        , -- | @since 1.0.0
          PEq
        )

-- | @since 1.4.0
instance DerivePlutusType PAssetClass where
    type DPTStrat _ = PlutusTypeData

-- | @since 1.0.0
passetClass ::
    forall (s :: S).
    Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
passetClass = phoistAcyclic $
    plam $ \cs tn ->
        pcon . PAssetClass $ pdcons # pdata cs # (pdcons # pdata tn # pdnil)

-- | @since 1.0.0
passetClassValueOf ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PValue keys amounts :--> PAssetClass :--> PInteger)
passetClassValueOf = phoistAcyclic $
    plam $ \val ac -> unTermCont $ do
        cs <- pletC (pfromData $ pfield @"currencySymbol" # ac)
        tn <- pletC (pfromData $ pfield @"tokenName" # ac)
        pure $ pvalueOf # val # cs # tn

-- | @since 1.0.0
pvalueOf ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PValue keys amounts :--> PCurrencySymbol :--> PTokenName :--> PInteger)
pvalueOf = phoistAcyclic $
    plam $ \val cs tn -> unTermCont $ do
        PValue m <- pmatchC val
        PMap m' <- pmatchC m
        res <- pmatchC (pfind # (go # cs) # m')
        case res of
            PNothing -> pure 0
            PJust inner -> do
                PMap innerMap <- pmatchC (pfromData $ psndBuiltin # inner)
                res2 <- pmatchC (pfind # (go # tn) # innerMap)
                case res2 of
                    PNothing -> pure 0
                    PJust answer -> pure . pfromData $ psndBuiltin # answer
  where
    go ::
        forall (a :: S -> Type) (b :: S -> Type) (s' :: S).
        (PIsData a, PEq a) =>
        Term s' (a :--> PBuiltinPair (PAsData a) (PAsData b) :--> PBool)
    go = phoistAcyclic $
        plam $ \target p -> unTermCont $ do
            a' <- pletC (pfromData $ pfstBuiltin # p)
            pure (target #== a')
