{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
-- TODO: This uses ClosedTerm in a return position together with let-bindings
-- for its skolem. This is only doable using _; as this is brittle and hard to
-- work with, it should be addressed, and the following removed.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- TODO: Remove this and replace with TermCont.
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- TODO: Either disable warning about orphans altogether or address the issue
-- requiring this unusual solution.
{-# OPTIONS_GHC -Wwarn=orphans #-}
{-# LANGUAGE TypeApplications #-}

-- | Utilities for working with monetary quantites in Plutarch
module Plutarch.Extra.Units.Quantity (
    pliftQuantity,
    assetClassValue,
    mkSingleValue,
    mkSingleValue',
    pvalue,

    -- * Exchange-rate conversions
    exchangeFromTruncate,
    exchangeToTruncate,
    exchangeFrom,
    exchangeTo,

    -- * PAssetClass/PTagged functions
    findValue,
    matchSingle,
    valueOfClass,
    matchValueAssets,

    -- * PValue utils
    precValue,
    pelimValue,
    pcmpMap,
    pgeqValue,
    pgeqValueEntry,
    psplitValue,
    plookup',

    -- * for testing
    unsafeMatchValueAssetsInternal,
    SomeAssetClass (SomeAssetClass),
) where

--------------------------------------------------------------------------------

import Data.Maybe (fromJust)
import Data.Tagged (Tagged, untag)
import GHC.Stack (HasCallStack)

--------------------------------------------------------------------------------

import Plutarch.Api.V1 (
    AmountGuarantees (),
    KeyGuarantees (Sorted),
    PCurrencySymbol,
    PMap,
    PTokenName,
    PValue (PValue),
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.DataRepr (HRec)
import Plutarch.DataRepr.Internal.Field (HRec (HCons, HNil))
import Plutarch.DataRepr.Internal.HList (Labeled (Labeled))
import Plutarch.Extra.Applicative (ppure)
import Plutarch.Extra.Tagged (PTagged (PTagged))
import "plutarch-extra" Plutarch.Extra.TermCont (
    pletC,
    pmatchC,
 )
import qualified Plutarch.Monadic as P

import qualified PlutusLedgerApi.V1.Value as Value
import qualified PlutusTx

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.TypeLits (Symbol)

-- import Liqwid.Plutarch (pfromList)

import Plutarch.Extra.Units.Rational (
    divRational,
    divTruncate,
    mulRational,
    mulTruncate,
 )
import Plutarch.Extra.Units.ScottAssetClass (
    AssetClass (AssetClass, name, symbol),
    PAssetClass (PAssetClass),
    (:>),
 )

--------------------------------------------------------------------------------
-- Plutarch Functions
-- Quantities

-- | `plift` for Tagged quantities (kind polymorphic)
pliftQuantity ::
    forall k (tag :: k).
    HasCallStack =>
    ClosedTerm (PTagged tag PRational) ->
    Tagged tag Rational
pliftQuantity term =
    fromJust $
        PlutusTx.fromData $
            plift (pforgetData $ pdata term)

-- | Version of assetClassValue for tagged AssetClass & Tagged
assetClassValue ::
    forall (unit :: Symbol).
    AssetClass unit ->
    Tagged unit Integer ->
    Value.Value
assetClassValue (AssetClass sym tk) q = Value.singleton sym tk $ untag q

-- | Exchange from one currency to another, truncating the result
exchangeFromTruncate ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged a PInteger
            :--> PTagged b PInteger
        )
exchangeFromTruncate =
    phoistAcyclic $
        plam $ \ex x -> P.do
            ppure #$ mulTruncate # pto ex # pto x

{- | Exchange from  one currency to another, truncating the result
 (inverse direction)
-}
exchangeToTruncate ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged b PInteger
            :--> PTagged a PInteger
        )
exchangeToTruncate =
    phoistAcyclic $
        plam $ \ex x -> P.do
            ppure #$ divTruncate # pto ex # pto x

-- | Convert between quantities of currencies using a Rational conversion value
exchangeFrom ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged a PInteger
            :--> PTagged b PRational
        )
exchangeFrom =
    phoistAcyclic $
        plam $ \ex x -> P.do
            ppure #$ mulRational # pto x # pto ex

-- | Convert between quantities of currencies, in the inverse direction
exchangeTo ::
    forall (a :: Symbol) (b :: Symbol) (s :: S).
    Term
        s
        ( PTagged (a :> b) PRational :--> PTagged b PInteger
            :--> PTagged a PRational
        )
exchangeTo =
    phoistAcyclic $
        plam $ \ex x ->
            ppure #$ divRational # pto x # pto ex

--------------------------------------------------------------------------------
-- Plutarch Functions: AssetClass

{- | Existential wrapper for `AssetClass`. Useful to use `AssetClass` as a key
 type in Map.
-}
data SomeAssetClass = forall (unit :: Symbol). SomeAssetClass (AssetClass unit)

instance Eq SomeAssetClass where
    (SomeAssetClass a) == (SomeAssetClass b) = a == coerce b

instance Ord SomeAssetClass where
    (SomeAssetClass a) `compare` (SomeAssetClass b) =
        let symbolOrder = a.symbol `compare` b.symbol
         in if symbolOrder /= EQ
                then symbolOrder
                else a.name `compare` b.name

{- | This function does not check if keys are duplicated or are in different
 order
-}
unsafeMatchValueAssetsInternal ::
    forall (k :: KeyGuarantees) (s :: S).
    Term s (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) ->
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

class HRecToList (xs :: [(Symbol, Type)]) (x :: Type) where
    hrecToList :: HRec xs -> [x]

instance HRecToList '[] (x :: Type) where
    hrecToList _ = []

-- | Converts type level lists of tagged assets back to dynamic-typed assets
instance
    forall (rest :: [(Symbol, Type)]) (name :: Symbol) (tag :: Symbol).
    HRecToList rest SomeAssetClass =>
    HRecToList
        ('(name, AssetClass tag) ': rest)
        SomeAssetClass
    where
    hrecToList (HCons (Labeled x) xs) = SomeAssetClass x : hrecToList xs

{- |
  Extracts amount of given PAssetClass from PValue. Behaves like a pattern
  match on PValue.  Not all AssetClasses need to be provided, only these that
  you are interested in.
  __Example__
  @
  example :: Term s PValueMap -> TermCont s ()
  example pval = do
    rec <- matchValueAssets @'['("ada", AssetClass "Ada")]
             pval
             (HCons (Labeled adaClass) HNil)
    -- or using operators
    rec2 <- matchValueAssets pval $ (Proxy @"ada" .|== adaClass) HNil
    let adaFromRec = rec.ada
  @
-}
matchValueAssets ::
    forall (input :: [(Symbol, Type)]) (s :: S).
    ( MatchValueAssetReferences input s
    , HRecToList input SomeAssetClass
    ) =>
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap 'Sorted PTokenName PInteger))
    ))) ->
    HRec input ->
    TermCont s (HRec (OutputMatchValueAssets input s))
matchValueAssets pvaluemap inputs = do
    -- nub reduces case Underlying being Ada also
    let sortedInputs = sort . nub . hrecToList $ inputs
    -- perform actuall pattern match and save references to Haskell's Map
    matchedMap <- unsafeMatchValueAssetsInternal pvaluemap sortedInputs
    -- reconstruct HRec with references saved on the matchedMap
    pure $ matchValueAssetReferences @input matchedMap inputs

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

--------------------------------------------------------------------------------

-- | Lookup the quantity of an AssetClass in a Value
valueOfClass ::
    forall
        (key :: KeyGuarantees)
        (amount :: AmountGuarantees)
        (unit :: Symbol)
        (s :: S).
    Term s (PAssetClass unit :--> PValue key amount :--> PTagged unit PInteger)
valueOfClass = phoistAcyclic $
    plam $ \cls val -> P.do
        (PAssetClass sym tk) <- pmatch cls
        ppure #$ precList (findValue sym tk) (const 0) #$ pto $ pto val

-- | Elimator for PValueMap
precValue ::
    forall r (k :: KeyGuarantees) s.
    ( Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) :--> r) ->
      Term s (PAsData PCurrencySymbol) ->
      Term s (PAsData PTokenName) ->
      Term s (PAsData PInteger) ->
      Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    ))) ->
      Term s r
    ) ->
    (Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) :--> r) -> Term s r) ->
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) :--> r)
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
      Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    ))) ->
      Term s r
    ) ->
    Term s r ->
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    ))) ->
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

{- | Finds a first matching PAssetClass in PValueMap when also returning the
 rest of the PValueMap
-}
matchOrTryRec ::
    forall (unit :: Symbol) (k :: KeyGuarantees) (s :: S).
    AssetClass unit ->
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) :--> PPair (PAsData PInteger) ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    ))))
matchOrTryRec requiredAsset = phoistAcyclic $
    plam $ \inputpvalue -> plet (pcon $ PPair (pdata 0) inputpvalue) $ \def ->
        precValue
            ( \self pcurrencySymbol pTokenName pint rest ->
                pif
                    ( pconstantData requiredAsset.symbol #== pcurrencySymbol
                        #&& pconstantData requiredAsset.name #== pTokenName
                    )
                    -- assetClass found - return its quantity and tail of PValueMap
                    (pcon $ PPair pint rest)
                    -- assetClass not found - skipping current position
                    (self # rest)
            )
            (const def)
            # inputpvalue

-- | Helper to construct the inner-mapping of a PValue
mkSingleValue ::
    forall (key :: KeyGuarantees) (tag :: Symbol) (s :: S).
    Term
        s
        ( PAsData PCurrencySymbol :--> PAsData PTokenName :--> PTagged tag PInteger
            :--> PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap key PTokenName PInteger))
        )
mkSingleValue = phoistAcyclic $
    plam $ \sym tk q ->
        ppairDataBuiltin
            # sym
            # pdata (pcon $ PMap $ pfromList [ppairDataBuiltin # tk # pdata (pto q)])

{- | Version of mkSingleValue with a haskell-level constant, hoisting with the
 applied arguments
-}
mkSingleValue' ::
    forall (tag :: Symbol) (k :: KeyGuarantees) (s :: S).
    AssetClass tag ->
    Term
        s
        ( PTagged tag PInteger
            :--> PBuiltinPair
                    (PAsData PCurrencySymbol)
                    (PAsData (PMap k PTokenName PInteger))
        )
mkSingleValue' (AssetClass sym tk) =
    phoistAcyclic $ mkSingleValue # pconstantData sym # pconstantData tk

-- | Construct a PValue from a PValueMap
pvalue ::
    forall (k :: KeyGuarantees) (amounts :: AmountGuarantees) (s :: S).
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) :--> PAsData (PValue k amounts))
pvalue = phoistAcyclic $ plam $ pdata . pcon . PValue . pcon . PMap

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
        let vList :: Term _  (PBuiltinList
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
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    )) :--> PInteger) ->
    Term
        s
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (PMap k PTokenName PInteger))
        ) ->
    Term s ( (PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap k PTokenName PInteger))
    ))) ->
    Term s PInteger
findValue sym tk self x xs = P.do
    pair <- plet x
    pif
        (pforgetData (pfstBuiltin # pair) #== pforgetData sym)
        (pelimList (matchSingle tk) perror $ pto $ pfromData (psndBuiltin # pair))
        (self # xs)

-- | Matches ((tokenName, value) : []), or error
matchSingle ::
    forall (s :: S).
    Term s (PAsData PTokenName) ->
    Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))) ->
    Term s PInteger
matchSingle tk x xs = P.do
    pair <- plet x
    pif
        (((pfstBuiltin # pair) #== tk) #&& (pnull # xs))
        (pfromData $ psndBuiltin # pair)
        perror

-- TODO: Remove me, once Seungheon's PR lands
pfromList ::
  forall (list :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PIsListLike list a) =>
  [Term s a] ->
  Term s (list a)
pfromList = foldr (\x xs -> pcons # x # xs) pnil
