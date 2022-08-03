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

import Data.Kind (Type)
import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Plutarch (
    DerivePlutusType (..),
    PlutusType,
    S,
    Term,
    pcon,
    phoistAcyclic,
    plam,
    unTermCont,
    (#),
    type (:-->),
 )
import Plutarch.Api.V2 (
    AmountGuarantees,
    KeyGuarantees,
  )
import Plutarch.Api.V1 (
  PCurrencySymbol,
    PTokenName,
    PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Bool (PBool, PEq ((#==)))
import Plutarch.Builtin (
    PAsData,
    PBuiltinPair,
    PIsData,
    pdata,
    pfromData,
    pfstBuiltin,
    psndBuiltin,
 )
import Plutarch.DataRepr (
    PDataFields,
    PDataRecord,
    PLabeledType ((:=)),
    PlutusTypeData,
    pdcons,
    pdnil,
    pfield,
 )
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Integer (PInteger)
import Plutarch.List (pfind)
import Plutarch.Maybe (PMaybe (PJust, PNothing))

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
          GHC.Generic
        )
    deriving anyclass
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
        )

-- | @since 1.4.0
instance DerivePlutusType PAssetClass where
    type DPTStrat _ = PlutusTypeData

-- | @since 1.0.0
deriving anyclass instance (PEq PAssetClass)

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
