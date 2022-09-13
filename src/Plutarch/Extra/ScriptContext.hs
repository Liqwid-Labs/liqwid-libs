{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Extra.ScriptContext (
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
    pfindTxInByTxOutRef,
    pvalidatorHashFromAddress,
    pscriptHashFromAddress,
    pisScriptAddress,
    pisPubKey,
    pfindOutputsToAddress,
    pfromPDatum,
    presolveOutputDatum,
    ptryResolveOutputDatum,
    ptryFromOutputDatum,
    pfromOutputDatum,
    pfromDatumHash,
    pfromInlineDatum,
) where

import Plutarch.Api.V1 (
    AmountGuarantees (NoGuarantees, NonZero, Positive),
    PCredential (PPubKeyCredential, PScriptCredential),
    PMap,
    PValidatorHash,
    PValue,
 )
import qualified Plutarch.Api.V1.AssocMap as AssocMap
import Plutarch.Api.V2 (
    KeyGuarantees (Sorted, Unsorted),
    PAddress (PAddress),
    PDatum,
    PDatumHash,
    PMaybeData,
    POutputDatum (PNoOutputDatum, POutputDatum, POutputDatumHash),
    PPubKeyHash,
    PScriptContext,
    PScriptPurpose (PSpending),
    PStakingCredential,
    PTxInInfo (PTxInInfo),
    PTxInfo,
    PTxOut (PTxOut),
    PTxOutRef,
 )
import Plutarch.Extra.AssetClass (PAssetClass, passetClassValueOf)
import Plutarch.Extra.Function ((#.*))
import Plutarch.Extra.Functor (PFunctor (pfmap))
import Plutarch.Extra.List (pfindJust)
import Plutarch.Extra.Maybe (pfromJust, pisJust, pjust, pnothing, ptraceIfNothing)
import Plutarch.Extra.TermCont (pletC, pmatchC)
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
            PJust res' -> res'
  where
    go ::
        forall (s' :: S).
        Term s' (PTxOutRef :--> PTxInInfo :--> PBool)
    go = phoistAcyclic $
        plam $ \tgt t -> unTermCont $ do
            x <- pletC (pfield @"outRef" # t)
            pure $ tgt #== x

{- | Determines if a given UTXO is spent.

    @since 1.1.0
-}
pisUTXOSpent :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pisUTXOSpent = phoistAcyclic $
    plam $ \oref inputs -> pisJust #$ pfindTxInByTxOutRef # oref # inputs

{- | Sum of all value at input.

    @since 1.1.0
-}
pvalueSpent ::
    forall (s :: S).
    Term s (PBuiltinList PTxInInfo :--> PValue 'Sorted 'Positive)
pvalueSpent = phoistAcyclic $
    plam $ \inputs ->
        pfoldr
            # plam
                ( \txInInfo' v ->
                    pmatch
                        txInInfo'
                        $ \(PTxInInfo txInInfo) ->
                            pmatch
                                (pfield @"resolved" # txInInfo)
                                (\(PTxOut o) -> pfield @"value" # o)
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
pisTokenSpent :: forall (s :: S). Term s (PAssetClass :--> PBuiltinList PTxInInfo :--> PBool)
pisTokenSpent =
    plam $ \tokenClass inputs ->
        0
            #< pfoldr @PBuiltinList
                # plam
                    ( \txInInfo' acc -> unTermCont $ do
                        PTxInInfo txInInfo <- pmatchC txInInfo'
                        PTxOut txOut' <- pmatchC $ pfromData $ pfield @"resolved" # txInInfo
                        let value = pfromData $ pfield @"value" # txOut'
                        pure $ acc + passetClassValueOf # value # tokenClass
                    )
                # 0
                # inputs

{- | Find the TxInInfo by a TxOutRef.

    @since 1.1.0
-}
pfindTxInByTxOutRef :: forall (s :: S). Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PMaybe PTxInInfo)
pfindTxInByTxOutRef = phoistAcyclic $
    plam $ \txOutRef inputs ->
        pfindJust
            # plam
                ( \r ->
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
ptxSignedBy :: forall (s :: S). Term s (PBuiltinList (PAsData PPubKeyHash) :--> PAsData PPubKeyHash :--> PBool)
ptxSignedBy = phoistAcyclic $
    plam $ \sigs sig -> pelem # sig # sigs

{- | Convert a 'PDatum' to the give type @a@.

 @since 3.0.3
-}
pfromPDatum ::
    forall (a :: S -> Type) (s :: S).
    PTryFrom PData a =>
    Term s (PDatum :--> a)
pfromPDatum = phoistAcyclic $ plam $ flip ptryFrom fst . pto

{- | Extract the datum from a 'POutputDatum'.

     @since 3.0.3
-}
presolveOutputDatum ::
    forall (keys :: KeyGuarantees) (s :: S).
    Term
        s
        ( POutputDatum
            :--> PMap keys PDatumHash PDatum
            :--> PMaybe PDatum
        )
presolveOutputDatum = phoistAcyclic $
    plam $ \od m -> pmatch od $ \case
        PNoOutputDatum _ ->
            ptrace "no datum" pnothing
        POutputDatum ((pfield @"outputDatum" #) -> datum) ->
            ptrace "inline datum" pjust # datum
        POutputDatumHash ((pfield @"datumHash" #) -> hash) ->
            ptrace "datum hash" $ AssocMap.plookup # hash # m

{- | As 'presolveOutputDatum', but error if there's no 'PDatum' to be had.

 @since 3.6.0
-}
ptryResolveOutputDatum ::
    forall (keys :: KeyGuarantees) (s :: S).
    Term s (POutputDatum :--> PMap keys PDatumHash PDatum :--> PDatum)
ptryResolveOutputDatum = phoistAcyclic $
    plam $ \od m ->
        ptraceIfNothing "ptryResolveOutputDatum: no PDatum" $ presolveOutputDatum # od # m

{- | Extract the datum from a 'POutputDatum' and convert it to the given type.

     @since 3.0.3
-}
ptryFromOutputDatum ::
    forall (a :: S -> Type) (s :: S).
    PTryFrom PData a =>
    Term
        s
        ( POutputDatum
            :--> PMap 'Unsorted PDatumHash PDatum
            :--> PMaybe a
        )
ptryFromOutputDatum =
    phoistAcyclic $
        (pfmap # pfromPDatum)
            #.* presolveOutputDatum

{- | Extract the datum from a 'POutputDatum' and convert it to the given type.
     This function will throw an error if for some reason it's not able to find
      the datum or convert it.

     @since 3.0.3
-}
pfromOutputDatum ::
    forall (a :: S -> Type) (s :: S).
    PTryFrom PData a =>
    Term
        s
        ( POutputDatum
            :--> PMap 'Unsorted PDatumHash PDatum
            :--> a
        )
pfromOutputDatum =
    phoistAcyclic $ pfromJust #.* ptryFromOutputDatum

{- | Extract the datum hash from a 'POutputDatum', throw an error if the given
     'POuptutDatum' doesn't contain a datum hash.

     @since 3.0.3
-}
pfromDatumHash :: forall (s :: S). Term s (POutputDatum :--> PDatumHash)
pfromDatumHash = phoistAcyclic $
    plam $
        flip pmatch $ \case
            POutputDatumHash ((pfield @"datumHash" #) -> hash) -> hash
            _ -> ptraceError "not a datum hash"

{- | Extract the inline datum from a 'POutputDatum', throw an error if the given
     'POuptutDatum' is not an inline datum.

     @since 3.0.3
-}
pfromInlineDatum :: forall (s :: S). Term s (POutputDatum :--> PDatum)
pfromInlineDatum = phoistAcyclic $
    plam $
        flip pmatch $ \case
            POutputDatum ((pfield @"outputDatum" #) -> datum) -> datum
            _ -> ptraceError "not an inline datum"

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
            _ -> pcon PNothing

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

{- | Get script hash from an Address.
     @since 1.3.0
-}
pscriptHashFromAddress :: forall (s :: S). Term s (PAddress :--> PMaybe PValidatorHash)
pscriptHashFromAddress = phoistAcyclic $
    plam $ \addr ->
        pmatch (pfromData $ pfield @"credential" # addr) $ \case
            PScriptCredential ((pfield @"_0" #) -> h) -> pcon $ PJust h
            _ -> pcon PNothing

{- | Return true if the given address is a script address.
     @since 1.3.0
-}
pisScriptAddress :: forall (s :: S). Term s (PAddress :--> PBool)
pisScriptAddress = phoistAcyclic $
    plam $ \addr -> pnot #$ pisPubKey #$ pfromData $ pfield @"credential" # addr

{- | Return true if the given credential is a pub-key-hash.
     @since 1.3.0
-}
pisPubKey :: forall (s :: S). Term s (PCredential :--> PBool)
pisPubKey = phoistAcyclic $
    plam $ \cred ->
        pmatch cred $ \case
            PScriptCredential _ -> pconstant False
            _ -> pconstant True

{- | Find all TxOuts sent to an Address
     @since 1.3.0
-}
pfindOutputsToAddress ::
    forall (s :: S).
    Term
        s
        ( PBuiltinList PTxOut
            :--> PAddress
            :--> PBuiltinList PTxOut
        )
pfindOutputsToAddress = phoistAcyclic $
    plam $ \outputs address' -> unTermCont $ do
        address <- pletC $ pdata address'
        pure $
            pfilter # plam (\txOut -> pfield @"address" # txOut #== address)
                # outputs
