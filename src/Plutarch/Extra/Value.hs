{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Plutarch.Extra.Value (
    -- * Creation
    passetClassDataValue,
    psingleValue,
    psingleValue',
    pvalue,

    -- * Queries
    padaOf,
    passetClassValueOf',
    passetClassValueOf,
    pmatchValueAssets,
    psplitValue,

    -- * Aggregation and elimination
    psymbolValueOf,
    precValue,
    pelimValue,

    -- * Comparison
    pbyClassComparator,
    pbySymbolComparator,
    pbyClassComparator',

    -- * Miscellaneous
    AddGuarantees,
    phasOnlyOneTokenOfCurrencySymbol,
) where

import Data.Coerce (coerce)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.TypeLits (Symbol)
import Plutarch.Api.V1 (
    AmountGuarantees (NonZero),
    PCurrencySymbol,
    PMap (PMap),
    PTokenName,
    PValue (PValue),
 )
import qualified Plutarch.Api.V1.AssocMap as AssocMap
import qualified Plutarch.Api.V1.Value as Value
import Plutarch.Api.V2 (
    AmountGuarantees (NoGuarantees, Positive),
    KeyGuarantees (Sorted),
 )
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.DataRepr.Internal.Field (HRec (HCons, HNil), Labeled (Labeled))
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.AssetClass (
    AssetClass (AssetClass, name, symbol),
    PAssetClass (PAssetClass),
    PAssetClassData,
 )
import Plutarch.Extra.List (pfromList, plookupAssoc, ptryElimSingle)
import Plutarch.Extra.Map (phandleMin)
import Plutarch.Extra.Maybe (pexpectJustC)
import Plutarch.Extra.Ord (PComparator, pfromOrdBy)
import Plutarch.Extra.Tagged (PTagged (PTagged))
import Plutarch.Extra.TermCont (pmatchC)

--------------------------------------------------------------------------------

----------------------------------------
-- Value Creation

{- | Create a 'PValue' that only contains a specific amount of tokens, described
 by a 'PAssetClassData'.

 @since 3.8.0
-}
passetClassDataValue ::
    forall (s :: S).
    Term s (PAssetClassData :--> PInteger :--> PValue 'Sorted 'NonZero)
passetClassDataValue = phoistAcyclic $
    plam $ \ac i ->
        pif
            (i #== 0)
            (ptraceError "passetClassDataValue: given zero argument, expecting nonzero.")
            ( let cs = pfield @"symbol" # ac
                  tn = pfield @"name" # ac
               in Value.psingleton # pfromData cs # pfromData tn # i
            )

{- | Helper to construct the \'inner mapping\' of a 'PValue'.

 @since 3.8.0
-}
psingleValue ::
    forall (key :: KeyGuarantees) (tag :: Symbol) (s :: S).
    Term
        s
        ( PAsData PCurrencySymbol :--> PAsData PTokenName :--> PTagged tag PInteger
            :--> PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap key PTokenName PInteger))
        )
psingleValue = phoistAcyclic $
    plam $ \sym tk q ->
        ppairDataBuiltin
            # sym
            # pdata (pcon $ PMap $ pfromList [ppairDataBuiltin # tk # pdata (pto q)])

{- | As 'psingleValue', but using a Haskell-level 'AssetClass'.

 @since 3.8.0
-}
psingleValue' ::
    forall (tag :: Symbol) (k :: KeyGuarantees) (s :: S).
    AssetClass tag ->
    Term
        s
        ( PTagged tag PInteger
            :--> PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
        )
psingleValue' (AssetClass sym tk) =
    phoistAcyclic $ psingleValue # pconstantData sym # pconstantData tk

{- | Construct a 'PValue' from its underlying representation.

 @since 3.8.0
-}
pvalue ::
    forall (k :: KeyGuarantees) (s :: S).
    Term
        s
        ( PBuiltinList
            ( PBuiltinPair
                (PAsData PCurrencySymbol)
                (PAsData (PMap k PTokenName PInteger))
            )
            :--> PAsData (PValue k 'NoGuarantees)
        )
pvalue = phoistAcyclic $ plam $ pdata . pcon . PValue . pcon . PMap

----------------------------------------
-- Value Querying, Extraction, and Matching

{- | Get the amount of ada of a 'PValue'.

   @since 3.8.0
-}
padaOf ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PValue keys amounts :--> PInteger)
padaOf = phoistAcyclic $
    plam $ \v ->
        Value.pvalueOf # v # pconstant "" # pconstant ""

{- | As 'passetClassValueOf', but using a Haskell-level 'AssetClass'.

 @since 3.8.0
-}
passetClassValueOf' ::
    forall (tag :: Symbol) (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    AssetClass tag ->
    Term s (PValue keys amounts :--> PInteger)
passetClassValueOf' (AssetClass sym token) =
    phoistAcyclic $
        plam $ \value ->
            Value.pvalueOf # value # pconstant sym # pconstant token

{- | Given a 'PAssetClass' and a 'PValue', look up the amount corresponding to
 that 'PAssetClass'.

 @since 3.8.0
-}
passetClassValueOf ::
    forall
        (key :: KeyGuarantees)
        (amount :: AmountGuarantees)
        (unit :: Symbol)
        (s :: S).
    Term s (PAssetClass unit :--> PValue key amount :--> PTagged unit PInteger)
passetClassValueOf = phoistAcyclic $
    plam $ \cls val -> pmatch cls $ \(PAssetClass sym tk) ->
        ppure #$ precList (findValue sym tk) (const 0) #$ pto $ pto val

{- | Extracts the amount given by the 'PAssetClass' from (the internal
 representation of) a 'PValue'.

 Intuitively, this acts as a \'pattern match\' on 'PValue'. You don't have to
 provide every asset class, only the ones you are interested in.

 = Example

 > example :: forall (s :: S) .
 >    Term s (PBuiltinList (
 >      PBuiltinPair (PAsData PCurrencySymbol)
 >                   (PAsData (PMap 'Sorted PTokenName PInteger))
 >      ) -> TermConst s ()
 > example rep = do
 >    rec <- matchValueAssets @'['("ada", AssetClass "Ada")]
 >              rep
 >              (HCons (Labeled adaClass) HNil)
 >    -- Or using operators
 >    rec2 <- matchValueAssets rep $ (Proxy @"ada" .|== adaClass) HNIl
 >    let adaFromRep = rec.ada

 @since 3.8.0
-}
pmatchValueAssets ::
    forall (input :: [(Symbol, Type)]) (s :: S).
    ( MatchValueAssetReferences input s
    , HRecToList input SomeAssetClass
    ) =>
    Term
        s
        ( PBuiltinList
            ( PBuiltinPair
                (PAsData PCurrencySymbol)
                (PAsData (PMap 'Sorted PTokenName PInteger))
            )
        ) ->
    HRec input ->
    TermCont s (HRec (OutputMatchValueAssets input s))
pmatchValueAssets pvaluemap inputs = do
    -- nub reduces case Underlying being Ada also
    let sortedInputs = sort . nub . hrecToList $ inputs
    -- perform actuall pattern match and save references to Haskell's Map
    matchedMap <- unsafeMatchValueAssetsInternal pvaluemap sortedInputs
    -- reconstruct HRec with references saved on the matchedMap
    pure $ matchValueAssetReferences @input matchedMap inputs

{- |
  Gets the first entry in the first item of the 'PValue' mapping & returns the
  rest of the 'PValue'.

  Fails if the 'PValue' is empty. In cases where we know that a 'PValue' contains
  Ada, such as in a 'PScriptContext', then this will function will split the
  Ada value - since the Ada entry comes first.

  NOTE: All properly normalized values will contain an Ada entry, even if
  that entry is 0.

  @since 3.8.0
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
        let vList = pto $ pto v
         in pcon $
                PPair
                    ( psndBuiltin #$ phead #$ pto $
                        pto $
                            pfromData
                                (psndBuiltin # (phead # vList))
                    )
                    (pcon $ PValue $ pcon $ PMap $ ptail # vList)

----------------------------------------
-- Value Aggregation and Elimination

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

{- | Eliminator for a sorted 'PValue'. The function argument will receive:

 * The smallest 'PCurrencySymbol' for which we have a mapping;
 * The smallest 'PTokenName' for which the corresponding \'inner map\' has a
 mapping;
 * The 'PInteger' associated with the above 'PTokenName'; and
 * The \'rest\'.

 If the \'inner map\' corresponding to the 'PCurrencySymbol' above contains
 only a single mapping, the \'rest\' will not contain a mapping for that
 'PCurrencySymbol'; otherwise, it will contain the rest of the \'inner map\'.

 = Note

 The \'nil case\' will be invoked in two situations:

 * When the 'PValue' has no entries; and
 * When the \'inner map\' corresponding to the first 'PCurrencySymbol' key has
 no entries.

 @since 3.8.0
-}
pelimValue ::
    forall (amounts :: AmountGuarantees) (r :: S -> Type) (s :: S).
    ( Term s PCurrencySymbol ->
      Term s PTokenName ->
      Term s PInteger ->
      Term s (PValue 'Sorted amounts) ->
      Term s r
    ) ->
    Term s r ->
    Term s (PValue 'Sorted amounts) ->
    Term s r
pelimValue whenCons whenNil xs = phandleMin (pto xs) whenNil $ \k v kvs ->
    phandleMin v whenNil $ \vk vv rest ->
        whenCons k vk vv . pcon . PValue $
            pif
                (AssocMap.pnull # rest)
                kvs
                (AssocMap.pinsert # k # rest # kvs)

{- | Eliminator for the inner type of a 'PValue'.

 @since 3.8.0
-}
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

----------------------------------------
-- Value Comparison

{- | Compare only on the basis of a particular 'PCurrencySymbol' and
 'PTokenName' entry.

 @since 3.8.0
-}
pbyClassComparator ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PTokenName :--> PComparator (PValue keys amounts))
pbyClassComparator = phoistAcyclic $
    plam $ \cs tn ->
        pfromOrdBy # plam (\pval -> Value.pvalueOf # pval # cs # tn)

{- | Compare only the entries corresponding to a particular 'PCurrencySymbol'.

 @since 3.8.0
-}
pbySymbolComparator ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PComparator (PValue keys amounts))
pbySymbolComparator = phoistAcyclic $
    plam $ \cs ->
        pfromOrdBy # plam (psymbolValueOf # cs #)

{- | As 'pbyClassComparator', but using a Haskell-level 'AssetClass' instead.

 @since 3.8.0
-}
pbyClassComparator' ::
    forall (tag :: Symbol) (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    AssetClass tag ->
    Term s (PComparator (PValue keys amounts))
pbyClassComparator' ac = pfromOrdBy # plam (passetClassValueOf' ac #)

----------------------------------------
-- Miscellaneous
-- TODO: Peter, 2022-09-21: There's a lot of typeclasses and instances that I
-- haven't looked at yet. I don't know how many of these belong in this module vs
-- should be factored out.

{- | Compute the guarantees known after adding two 'PValue's.

   @since 1.1.0
-}
type family AddGuarantees (a :: AmountGuarantees) (b :: AmountGuarantees) where
    AddGuarantees 'Positive 'Positive = 'Positive
    AddGuarantees _ _ = 'NoGuarantees

{- | Returns 'PTrue' if the entire argument 'PValue' contains /exactly/ one
 token of the argument 'PCurrencySymbol'.

 @since 1.3.0
-}
phasOnlyOneTokenOfCurrencySymbol ::
    forall (keys :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s (PCurrencySymbol :--> PValue keys amounts :--> PBool)
phasOnlyOneTokenOfCurrencySymbol = phoistAcyclic $
    plam $ \cs vs ->
        psymbolValueOf # cs # vs #== 1
            #&& (plength #$ pto $ pto $ pto vs) #== 1

-- Helpers

-- Cons case for QTokenValue, with recursive fixpoint
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
        (ptryElimSingle go . pto . pfromData $ psndBuiltin # pair)
        (self # xs)
  where
    go ::
        Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
        Term s PInteger
    go kv =
        pif
            (pfstBuiltin # kv #== tk)
            (pfromData $ psndBuiltin # kv)
            (ptraceError "findValue: Unexpectedly missing result.")

unsafeMatchValueAssetsInternal ::
    forall (k :: KeyGuarantees) (s :: S).
    Term
        s
        ( PBuiltinList
            ( PBuiltinPair
                (PAsData PCurrencySymbol)
                (PAsData (PMap k PTokenName PInteger))
            )
        ) ->
    [SomeAssetClass] ->
    TermCont s (Map SomeAssetClass (Term s (PAsData PInteger)))
unsafeMatchValueAssetsInternal _ [] = pure Map.empty
unsafeMatchValueAssetsInternal
    inputpvalue
    (sa@(SomeAssetClass someAsset) : rest) = do
        (PPair pint newValueMap) <-
            TermCont . pmatch $
                matchOrTryRec someAsset
                    # inputpvalue
        remaining <- unsafeMatchValueAssetsInternal newValueMap rest
        pure $ Map.insert sa pint remaining

-- This function does not check if keys are duplicated or are in different
-- order
class HRecToList (xs :: [(Symbol, Type)]) (x :: Type) where
    hrecToList :: HRec xs -> [x]

-- Existential wrapper for `AssetClass`. Useful to use `AssetClass` as a key
-- type in Map.
data SomeAssetClass = forall (unit :: Symbol). SomeAssetClass (AssetClass unit)

instance Eq SomeAssetClass where
    (SomeAssetClass a) == (SomeAssetClass b) = a == coerce b

instance Ord SomeAssetClass where
    (SomeAssetClass a) `compare` (SomeAssetClass b) =
        let symbolOrder = symbol a `compare` symbol b
         in if symbolOrder /= EQ
                then symbolOrder
                else name a `compare` name b

instance HRecToList '[] (x :: Type) where
    hrecToList _ = []

-- Converts type level lists of tagged assets back to dynamic-typed assets
instance
    forall (rest :: [(Symbol, Type)]) (name :: Symbol) (tag :: Symbol).
    HRecToList rest SomeAssetClass =>
    HRecToList
        ('(name, AssetClass tag) ': rest)
        SomeAssetClass
    where
    hrecToList (HCons (Labeled x) xs) = SomeAssetClass x : hrecToList xs

-- | Associates given Symbol of AssetClass to PTagged tag PInteger
type family OutputMatchValueAssets (ps :: [(Symbol, Type)]) (s :: S) where
    OutputMatchValueAssets '[] _ = '[]
    OutputMatchValueAssets ('(name, AssetClass tag) ': rest) s =
        '( name
         , Term
            s
            (PAsData (PTagged tag PInteger))
         )
            ': OutputMatchValueAssets rest s

class MatchValueAssetReferences (input :: [(Symbol, Type)]) (s :: S) where
    matchValueAssetReferences ::
        Map SomeAssetClass (Term s (PAsData PInteger)) ->
        HRec input ->
        HRec (OutputMatchValueAssets input s)

instance MatchValueAssetReferences '[] (s :: S) where
    matchValueAssetReferences _ _ = HNil

instance
    forall
        (name :: Symbol)
        (classSymbol :: Symbol)
        (is :: [(Symbol, Type)])
        (s :: S).
    MatchValueAssetReferences is s =>
    MatchValueAssetReferences
        ('(name, AssetClass classSymbol) ': is)
        s
    where
    matchValueAssetReferences valueMap (HCons (Labeled cls) rest) =
        HCons
            ( Labeled @name
                ( pdata $
                    pcon $
                        PTagged $
                            pfromData (valueMap Map.! SomeAssetClass cls)
                )
            )
            (matchValueAssetReferences valueMap rest)

matchOrTryRec ::
    forall (unit :: Symbol) (k :: KeyGuarantees) (s :: S).
    AssetClass unit ->
    Term
        s
        ( PBuiltinList
            ( PBuiltinPair
                (PAsData PCurrencySymbol)
                (PAsData (PMap k PTokenName PInteger))
            )
            :--> PPair
                    (PAsData PInteger)
                    ( PBuiltinList
                        ( PBuiltinPair
                            (PAsData PCurrencySymbol)
                            (PAsData (PMap k PTokenName PInteger))
                        )
                    )
        )
matchOrTryRec requiredAsset = phoistAcyclic $
    plam $ \inputpvalue -> plet (pcon $ PPair (pdata 0) inputpvalue) $ \def ->
        precValue
            ( \self pcurrencySymbol pTokenName pint rest ->
                pif
                    ( pconstantData (symbol requiredAsset) #== pcurrencySymbol
                        #&& pconstantData (name requiredAsset) #== pTokenName
                    )
                    -- assetClass found - return its quantity and tail of PValueMap
                    (pcon $ PPair pint rest)
                    -- assetClass not found - skipping current position
                    (self # rest)
            )
            (const def)
            # inputpvalue
