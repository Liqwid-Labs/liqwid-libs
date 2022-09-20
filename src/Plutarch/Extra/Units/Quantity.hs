{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- TODO: This uses ClosedTerm in a return position together with let-bindings
-- for its skolem. This is only doable using _; as this is brittle and hard to
-- work with, it should be addressed, and the following removed.
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- TODO: Remove this and replace with TermCont.
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- TODO: Either disable warning about orphans altogether or address the issue
-- requiring this unusual solution.
{-# OPTIONS_GHC -Wwarn=orphans #-}

-- | Utilities for working with monetary quantites in Plutarch
module Plutarch.Extra.Units.Quantity (
    pliftQuantity,
    assetClassValue,
    mkSingleValue,
    mkSingleValue',
    matchValueAssets,

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
    KeyGuarantees (Sorted),
    PCurrencySymbol,
    PMap,
    PTokenName,
 )
import Plutarch.Api.V1.AssocMap (PMap (PMap))
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.DataRepr (HRec)
import Plutarch.DataRepr.Internal.Field (HRec (HCons, HNil))
import Plutarch.DataRepr.Internal.HList (Labeled (Labeled))
import Plutarch.Extra.Tagged (PTagged (PTagged))
import qualified PlutusLedgerApi.V1.Value as Value
import qualified PlutusTx

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.TypeLits (Symbol)
import Plutarch.Extra.AssetClass (
    AssetClass (AssetClass, name, symbol),
 )

import Plutarch.Extra.Value (precValue)

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

{- | Finds a first matching PAssetClass in PValueMap when also returning the
 rest of the PValueMap
-}
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

-- TODO: Remove me, once Seungheon's PR lands
pfromList ::
    forall (list :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    (PIsListLike list a) =>
    [Term s a] ->
    Term s (list a)
pfromList = foldr (\x xs -> pcons # x # xs) pnil
