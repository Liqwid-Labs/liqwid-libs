{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

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

    -- * PValue utils
    passetClassValueOf,
    findValue,
    pvalue,
    matchSingle,
    precValue,
    pelimValue,
    pcmpMap,
    pgeqValue,
    pgeqValueEntry,
    psplitValue,
    plookup',
) where

import GHC.TypeLits (Symbol)
import Plutarch.Api.V1 (
    PCurrencySymbol,
    PMap (PMap),
    PTokenName,
    PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V2 (
    AmountGuarantees (NoGuarantees, Positive),
    KeyGuarantees (Sorted),
 )
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.AssetClass (PAssetClass (PAssetClass), PAssetClassData)
import Plutarch.Extra.List (plookupAssoc)
import Plutarch.Extra.Maybe (pexpectJustC)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))

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

-- | Create a `PValue` that only contains specific amount tokens of the given assetclass.
passetClassValue ::
    forall (s :: S) (keys :: KeyGuarantees) (amounts :: AmountGuarantees).
    Term s (PAssetClassData :--> PInteger :--> PValue keys amounts)
passetClassValue = phoistAcyclic $
    plam $ \ac i -> unTermCont $ do
        cs <- pletC (pfield @"symbol" # ac)
        tn <- pletC (pfield @"name" # ac)
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
            m' <- pexpectJustC 0 (plookupAssoc # pfstBuiltin # psndBuiltin # pdata sym # value)
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

-- | Elimator for PValueMap
precValue ::
    forall r (k :: KeyGuarantees) s.
    ( Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
            :--> r
        ) ->
      Term s (PAsData PCurrencySymbol) ->
      Term s (PAsData PTokenName) ->
      Term s (PAsData PInteger) ->
      Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
        ) ->
      Term s r
    ) ->
    ( Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
            :--> r
        ) ->
      Term s r
    ) ->
    Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
            :--> r
        )
precValue mcons =
    precList
        ( \self symbolMap symbolMaps ->
            pelimList
                ( \tokenMap tokenMaps ->
                    mcons
                        self
                        (pfstBuiltin # symbolMap)
                        (pfstBuiltin # tokenMap)
                        (psndBuiltin # tokenMap)
                        ( pif
                            (pnull # tokenMaps)
                            symbolMaps
                            ( pcons
                                # ( ppairDataBuiltin
                                        # (pfstBuiltin # symbolMap)
                                        # pdata (pcon (PMap tokenMaps))
                                  )
                                # symbolMaps
                            )
                        )
                )
                (self # symbolMaps)
                (pto $ pfromData $ psndBuiltin # symbolMap)
        )

-- | Elimator for PValueMap
pelimValue ::
    forall (r :: S -> Type) (k :: KeyGuarantees) (s :: S).
    ( Term s (PAsData PCurrencySymbol) ->
      Term s (PAsData PTokenName) ->
      Term s (PAsData PInteger) ->
      Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
        ) ->
      Term s r
    ) ->
    Term s r ->
    Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
        ) ->
    Term s r
pelimValue mcons mnil =
    pelimList
        ( \symbolMap symbolMaps ->
            pelimList
                ( \tokenMap tokenMaps ->
                    mcons
                        (pfstBuiltin # symbolMap)
                        (pfstBuiltin # tokenMap)
                        (psndBuiltin # tokenMap)
                        ( pif
                            (pnull # tokenMaps)
                            symbolMaps
                            ( pcons
                                # ( ppairDataBuiltin # (pfstBuiltin # symbolMap)
                                        # pdata (pcon (PMap tokenMaps))
                                  )
                                # symbolMaps
                            )
                        )
                )
                mnil
                (pto $ pfromData $ psndBuiltin # symbolMap)
        )
        mnil

{- |
  Returns True if the provided comparison function returns True for all
    entries in the first map with the same key in the second.
-}
pcmpMap ::
    forall (k :: S -> Type) (v :: S -> Type) (s :: S).
    ( POrd k
    , PIsData k
    ) =>
    Term
        s
        ( (PAsData k :--> PAsData v :--> PAsData v :--> PBool)
            :--> PMap 'Sorted k v
            :--> PMap 'Sorted k v
            :--> PBool
        )
pcmpMap =
    pfix
        # plam
            ( \self comp xs ys -> unTermCont $ do
                pure $
                    pelimList
                        ( \yHead yNext -> unTermCont $ do
                            needle <- pletC $ pfstBuiltin # yHead
                            find <-
                                pmatchC $
                                    plookup'
                                        # pfromData needle
                                        # xs
                            case find of
                                (PJust pair) -> do
                                    (PPair xEntry xNext) <- pmatchC pair
                                    yEntry :: Term s (PAsData v) <-
                                        pletC $
                                            psndBuiltin
                                                # yHead
                                    pure $
                                        pif
                                            (comp # needle # xEntry # yEntry)
                                            (self # comp # xNext # pcon (PMap yNext))
                                            (pcon PFalse)
                                PNothing -> pure $ pcon PFalse
                        )
                        (pcon PTrue)
                        (pto ys)
            )

{- |
  Gets the first entry in the first item of the `PValue` mapping & returns the
  rest of the PValue.

  Fails if the PValue is empty. In cases where we know that a PValue contains
  Ada, such as in the ScriptContext, then this will function will split the
  Ada value - since the Ada entry comes first.
-}
psplitValue ::
    forall (v :: AmountGuarantees) (s :: S).
    Term
        s
        ( PValue 'Sorted v
            :--> PPair
                    (PAsData PInteger)
                    (PValue 'Sorted v)
        )
psplitValue = phoistAcyclic $
    plam $ \v ->
        let vList ::
                Term
                    _
                    ( PBuiltinList
                        ( PBuiltinPair
                            (PAsData PCurrencySymbol)
                            (PAsData (PMap 'Sorted PTokenName PInteger))
                        )
                    )

            vList = pto $ pto v
         in pcon $
                PPair
                    ( psndBuiltin #$ phead #$ pto $
                        pto $
                            pfromData
                                (psndBuiltin # (phead # vList))
                    )
                    (pcon $ PValue $ pcon $ PMap $ ptail # vList)

{- |
  Returns True if all entries in the first Value present in the second Value
  are >=.
-}
pgeqValue ::
    forall (v :: AmountGuarantees) (s :: S).
    Term
        s
        ( PAsData (PValue 'Sorted v)
            :--> PAsData (PValue 'Sorted v)
            :--> PBool
        )
pgeqValue = phoistAcyclic $
    plam $ \x y ->
        x #== y
            #|| pcmpMap # pgeqValueEntry # pto (pfromData x) # pto (pfromData y)

{- |
  Returns True if all entries in the first Map are >= the values in the second.
-}
pgeqValueEntry ::
    forall (s :: S).
    Term
        s
        ( PAsData PCurrencySymbol
            :--> PAsData (PMap 'Sorted PTokenName PInteger)
            :--> PAsData (PMap 'Sorted PTokenName PInteger)
            :--> PBool
        )
pgeqValueEntry = phoistAcyclic $
    plam $ \_sym x y ->
        x #== y
            #|| (pcmpMap # plam (const pgeqIntegerData) # pfromData x # pfromData y)
  where
    pgeqIntegerData = phoistAcyclic $ plam $ \a b -> pfromData b #<= pfromData a

{- | `plookup` but also returning the tail of the list, and relying on
 sorted order
-}
plookup' ::
    forall (k :: S -> Type) (v :: (S -> Type)) (s :: S).
    ( POrd k
    , PIsData k
    ) =>
    Term
        s
        ( k
            :--> PMap 'Sorted k v
            :--> PMaybe (PPair (PAsData v) (PMap 'Sorted k v))
        )
plookup' = phoistAcyclic $
    plam $ \needle ->
        pfix #$ plam $ \self (xs :: Term _ (PMap 'Sorted k b)) ->
            pelimList
                ( \y ys -> unTermCont $ do
                    current <- pletC $ pfromData $ pfstBuiltin # y
                    let ysMap = pcon $ PMap ys
                    pure $
                        pif
                            (current #== needle)
                            (pcon $ PJust $ pcon $ PPair (psndBuiltin # y) ysMap)
                            (pif (needle #< current) (pcon PNothing) (self # ysMap))
                )
                (pcon PNothing)
                (pto xs)

-- | Cons case for QTokenValue, with recursive fixpoint
findValue ::
    forall (k :: KeyGuarantees) (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PTokenName) ->
    Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
            :--> PInteger
        ) ->
    Term
        s
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        ) ->
    Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
        ) ->
    Term s PInteger
findValue sym tk self x xs = plet x $ \pair ->
    pif
        (pforgetData (pfstBuiltin # pair) #== pforgetData sym)
        (pelimList (matchSingle tk) perror $ pto $ pfromData (psndBuiltin # pair))
        (self # xs)

-- | Lookup the quantity of an AssetClass in a Value
passetClassValueOf ::
    forall
        (key :: KeyGuarantees)
        (amount :: AmountGuarantees)
        (unit :: Symbol)
        (s :: S).
    Term s (PAssetClass unit :--> PValue key amount :--> PTagged unit PInteger)
passetClassValueOf = phoistAcyclic $
    plam $ \cls val -> unTermCont $ do
        (PAssetClass sym tk) <- pmatchC cls
        pure $ ppure #$ precList (findValue sym tk) (const 0) #$ pto $ pto val

-- | Construct a PValue from a PValueMap
pvalue ::
    forall (k :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term
        s
        ( PBuiltinList
                ( PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
                )
            :--> PAsData (PValue k amounts)
        )
pvalue = phoistAcyclic $ plam $ pdata . pcon . PValue . pcon . PMap

-- | Matches ((tokenName, value) : []), or error
matchSingle ::
    forall (s :: S).
    Term s (PAsData PTokenName) ->
    Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PInteger
matchSingle tk x xs = plet x $ \pair ->
    pif
        (((pfstBuiltin # pair) #== tk) #&& (pnull # xs))
        (pfromData $ psndBuiltin # pair)
        perror
