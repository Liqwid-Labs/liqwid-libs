{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Api.V1.ScriptContext (
    paddressFromValidatorHash,
    paddressFromPubKeyHash,
    pownTxOutRef,
    pownTxInfo,
    pownValue,
    pownMintValue,
    pownInput,
    pisTokenSpent,
    pisUTXOSpent,
    pvalueSpent,
    ptxSignedBy,
    ptryFindDatum,
    pfindDatum,
    pfindTxInByTxOutRef,
    pvalidatorHashFromAddress,
) where

import Data.Kind (Type)
import Plutarch (PCon (..), S, Term, phoistAcyclic, plam, plet, pmatch, pto, unTermCont, (#), (#$), (:-->))
import Plutarch.Api.V1 (
    AmountGuarantees (..),
    KeyGuarantees (..),
    PAddress (PAddress),
    PCredential (..),
    PDatum,
    PDatumHash,
    PMaybeData,
    PPubKeyHash,
    PScriptContext,
    PScriptPurpose (PSpending),
    PStakingCredential,
    PTuple,
    PTxInInfo (..),
    PTxInfo,
    PTxOut (..),
    PTxOutRef,
    PValidatorHash,
    PValue,
 )
import Plutarch.Api.V1.AssetClass (PAssetClass (..), passetClassValueOf)
import Plutarch.Bool (PBool, POrd (..), pif, (#==))
import Plutarch.Builtin (PAsData, PBuiltinList, PData, pdata, pfromData)
import Plutarch.DataRepr (pdcons, pdnil, pfield)
import Plutarch.Extra.List (pfirstJust, plookupTuple)
import Plutarch.Extra.Maybe (pisJust)
import Plutarch.Extra.TermCont (pletC, pmatchC, ptryFromC)
import Plutarch.Lift (pconstant)
import Plutarch.List (pelem, pfind, pfoldr)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Trace (ptraceError)
import Plutarch.TryFrom (PTryFrom)
import Plutarch.Unsafe (punsafeCoerce)

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
    Term s (PScriptContext :--> PValue 'Sorted 'Positive)
pownValue = phoistAcyclic $
    plam $ \sc -> unTermCont $ do
        input <- pletC (pownInput # sc)
        pure $ pfield @"value" # (pfield @"resolved" # input)

pownMintValue ::
    forall (s :: S).
    Term s (PScriptContext :--> PValue 'Sorted 'NoGuarantees)
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

{- | Determines if a given UTXO is spent.

    @since 1.1.0
-}
pisUTXOSpent :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
pisUTXOSpent = phoistAcyclic $
    plam $ \oref inputs -> pisJust #$ pfindTxInByTxOutRef # oref # inputs

{- | Sum of all value at input.

    @since 1.1.0
-}
pvalueSpent ::
    forall (s :: S).
    Term s (PBuiltinList (PAsData PTxInInfo) :--> PValue 'Sorted 'Positive)
pvalueSpent = phoistAcyclic $
    plam $ \inputs ->
        pfoldr
            # plam
                ( \txInInfo' v ->
                    pmatch
                        (pfromData txInInfo')
                        $ \(PTxInInfo txInInfo) ->
                            pmatch
                                (pfield @"resolved" # txInInfo)
                                (\(PTxOut o) -> pfromData $ pfield @"value" # o)
                                <> v
                )
            -- TODO: This should be possible without coercions, but I can't figure out the types atm.
            # punsafeCoerce (pconstant mempty :: Term _ (PValue 'Unsorted 'NonZero))
            # inputs

{- | Check if a particular asset class has been spent in the input list.

     When using this as an authority check, you __MUST__ ensure the authority
     knows how to ensure its end of the contract.

    @since 1.1.0
-}
pisTokenSpent :: forall {s :: S}. Term s (PAssetClass :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
pisTokenSpent =
    plam $ \tokenClass inputs ->
        0
            #< pfoldr @PBuiltinList
                # plam
                    ( \txInInfo' acc -> unTermCont $ do
                        PTxInInfo txInInfo <- pmatchC (pfromData txInInfo')
                        PTxOut txOut' <- pmatchC $ pfromData $ pfield @"resolved" # txInInfo
                        let value = pfromData $ pfield @"value" # txOut'
                        pure $ acc + passetClassValueOf # value # tokenClass
                    )
                # 0
                # inputs

{- | Find the TxInInfo by a TxOutRef.

    @since 1.1.0
-}
pfindTxInByTxOutRef :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = phoistAcyclic $
    plam $ \txOutRef inputs ->
        pfirstJust
            # plam
                ( \txInInfo' ->
                    plet (pfromData txInInfo') $ \r ->
                        pmatch r $ \(PTxInInfo txInInfo) ->
                            pif
                                (pdata txOutRef #== pfield @"outRef" # txInInfo)
                                (pcon (PJust r))
                                (pcon PNothing)
                )
            #$ inputs

{- | Check if a PubKeyHash signs this transaction.

    @since 1.1.0
-}
ptxSignedBy :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
    plam $ \sigs sig -> pelem # sig # sigs

{- | Find a datum with the given hash.

    @since 1.1.0
-}
pfindDatum :: Term s (PDatumHash :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
    plam $ \datumHash datums -> plookupTuple # datumHash # datums

{- | Find a datum with the given hash, and `ptryFrom` it.

    @since 1.1.0
-}
ptryFindDatum ::
    forall (a :: S -> Type) (s :: S).
    PTryFrom PData a =>
    Term s (PDatumHash :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PMaybe a)
ptryFindDatum = phoistAcyclic $
    plam $ \datumHash inputs ->
        pmatch (pfindDatum # datumHash # inputs) $ \case
            PNothing -> pcon PNothing
            PJust datum -> unTermCont $ do
                (datum', _) <- ptryFromC (pto datum)
                pure $ pcon (PJust datum')

{- | Find a validatorhash from a given address.

    @since 1.1.0
-}
pvalidatorHashFromAddress ::
    forall (s :: S).
    Term s (PAddress :--> PMaybe PValidatorHash)
pvalidatorHashFromAddress = phoistAcyclic $
    plam $ \addr ->
        pmatch (pfromData $ pfield @"credential" # addr) $ \case
            PScriptCredential ((pfield @"_0" #) -> vh) -> pcon $ PJust vh
            _ -> pcon $ PNothing

{- | Construct an address from a @PValidatorHash@ and maybe a
@PStakingCredential@

    @since 1.1.0
-}
paddressFromValidatorHash ::
    forall (s :: S).
    Term s (PValidatorHash :--> PMaybeData PStakingCredential :--> PAddress)
paddressFromValidatorHash = plam $ \valHash stakingCred ->
    pcon . PAddress $
        pdcons # pdata (pcon $ PScriptCredential (pdcons # pdata valHash # pdnil))
            #$ pdcons # pdata stakingCred
            #$ pdnil

{- | Constuct an address (with a staking credential) from a @PPubKeyHash@
and maybe a @PStakingCredential

    @since 1.1.0
-}
paddressFromPubKeyHash ::
    forall (s :: S).
    Term s (PPubKeyHash :--> PMaybeData PStakingCredential :--> PAddress)
paddressFromPubKeyHash = plam $ \pkh stakingCred ->
    pcon . PAddress $
        pdcons # pdata (pcon $ PPubKeyCredential (pdcons # pdata pkh # pdnil))
            #$ pdcons # pdata stakingCred
            #$ pdnil
