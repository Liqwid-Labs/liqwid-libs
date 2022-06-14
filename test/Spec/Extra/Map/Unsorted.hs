{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.Extra.Map.Unsorted (tests) where

--------------------------------------------------------------------------------

import Spec.Utils (
    genKeySet,
    genNonIdenticalKeySets,
    genSortedMapWithKeys,
    genUnsortedMapWithKeys,
    shrinkMap,
    sortMap,
 )
import Test.QuickCheck (
    Gen,
    Property,
    shrinkNothing,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Plutarch.Property (classifiedPropertyNative, peqPropertyNative')
import Test.Tasty.QuickCheck (
    testProperty,
 )

--------------------------------------------------------------------------------

import qualified Data.Map as M
import Data.Universe (Finite, Universe)
import qualified GHC.Generics as GHC

--------------------------------------------------------------------------------

import Plutarch (Term, phoistAcyclic, plam, (#), type (:-->))
import Plutarch.Api.V1.AssocMap (KeyGuarantees (..), PMap)
import Plutarch.Bool (PBool)
import Plutarch.Builtin (PBuiltinPair, pfstBuiltin, psndBuiltin)
import Plutarch.Integer (PInteger)
import Plutarch.Unsafe (punsafeCoerce)
import qualified PlutusTx.AssocMap as AssocMap

--------------------------------------------------------------------------------

import Plutarch.Extra.Map.Unsorted (pkeysEqual, psort, punionWith)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testProperty "'pkeysEquals' can tell whether two maps have the same key set or not" prop_keysEqualCorrect
    , testProperty "'punionWith' merges two sorted maps correctly" prop_unionMapsCorrect
    , testProperty "'psort' sorts unsorted maps as expcted" prop_psortCorrect
    ]

--------------------------------------------------------------------------------

-- | The property of 'psort' to make an unsorted map sorted.
prop_psortCorrect :: Property
prop_psortCorrect = peqPropertyNative' sortMap generator shrinkMap definition
  where
    generator :: Gen (AssocMap.Map Integer Integer)
    generator = genKeySet >>= genUnsortedMapWithKeys

    definition :: Term _ (PMap 'Unsorted PInteger PInteger :--> PMap 'Unsorted PInteger PInteger)
    definition = phoistAcyclic $ plam $ \m -> punsafeCoerce $ psort # m

--------------------------------------------------------------------------------

data KeysEqualCase = KeysAreEqual | KeysAreNotEqual
    deriving stock (GHC.Generic)
    deriving stock (Show, Enum, Bounded, Eq)
    deriving anyclass (Universe, Finite)

-- | The property of 'pkeysEqual' to determine whether two maps have the same keys or not.
prop_keysEqualCorrect :: Property
prop_keysEqualCorrect = classifiedPropertyNative generator shrinkNothing expected classifier definition
  where
    generator ::
        KeysEqualCase ->
        Gen (AssocMap.Map Integer Integer, AssocMap.Map Integer Integer)
    generator = \case
        KeysAreEqual -> do
            keys <- genKeySet
            a <- genSortedMapWithKeys keys
            b <- genSortedMapWithKeys keys
            return (a, b)
        KeysAreNotEqual -> do
            (ka, kb) <- genNonIdenticalKeySets
            (,)
                <$> genSortedMapWithKeys ka
                <*> genSortedMapWithKeys kb

    expected :: (AssocMap.Map Integer Integer, AssocMap.Map Integer Integer) -> Maybe Bool
    expected (AssocMap.keys -> a, AssocMap.keys -> b) = Just $ a == b

    classifier :: (AssocMap.Map Integer Integer, AssocMap.Map Integer Integer) -> KeysEqualCase
    classifier (AssocMap.keys -> a, AssocMap.keys -> b)
        | a == b = KeysAreEqual
        | otherwise = KeysAreNotEqual

    definition ::
        Term
            _
            ( PBuiltinPair
                (PMap 'Unsorted PInteger PInteger)
                (PMap 'Unsorted PInteger PInteger)
                :--> PBool
            )
    definition = phoistAcyclic $
        plam $ \pair ->
            let a = pfstBuiltin # pair
                b = psndBuiltin # pair
             in pkeysEqual # a # b

--------------------------------------------------------------------------------

-- | The property of 'punionMap' to merge two maps into one sorted map correctly.
prop_unionMapsCorrect :: Property
prop_unionMapsCorrect = peqPropertyNative' expected generator shrinkNothing definition
  where
    generator ::
        Gen (AssocMap.Map Integer Integer, AssocMap.Map Integer Integer)
    generator = do
        keysA <- genKeySet
        keysB <- genKeySet
        (,)
            <$> genSortedMapWithKeys keysA
            <*> genSortedMapWithKeys keysB

    expected ::
        ( AssocMap.Map Integer Integer
        , AssocMap.Map Integer Integer
        ) ->
        AssocMap.Map Integer Integer
    expected (AssocMap.toList -> a, AssocMap.toList -> b) =
        let ma = M.fromList a
            mb = M.fromList b

            unionF :: Integer -> Integer -> Integer
            unionF = (+)

            mu = M.unionWith unionF ma mb
         in AssocMap.fromList $ M.toList mu

    definition ::
        Term
            _
            ( PBuiltinPair
                (PMap 'Unsorted PInteger PInteger)
                (PMap 'Unsorted PInteger PInteger)
                :--> PMap 'Unsorted PInteger PInteger
            )
    definition = phoistAcyclic $
        plam $ \pair ->
            let a = pfstBuiltin # pair
                b = psndBuiltin # pair

                unionF :: Term _ (PInteger :--> PInteger :--> PInteger)
                unionF = phoistAcyclic $ plam (+)
             in punsafeCoerce $ punionWith # unionF # a # b
