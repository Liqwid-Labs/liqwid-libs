{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental
 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Lift (
    PConstantDecl (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
    PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import Plutarch.Test.QuickCheck

import GHC.Generics (Generic)
import Generics.SOP (NP (..))
import Test.QuickCheck (
    NonNegative (NonNegative),
    arbitrary,
    getNonNegative,
    shrink,
 )
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck

-- This is the "correct" implmentation of reversing function.
preverseCorrect ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    PIsListLike t a =>
    Term s (t a :--> t a)
preverseCorrect = phoistAcyclic $ pfoldl # plam (\ys y -> pcons # y # ys) # pnil

-- This is the "wrong" implmentation of reversing function. It will
-- return the input.
preverseWrong ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    PIsListLike t a =>
    Term s (t a :--> t a)
preverseWrong = plam id

-- Haskell equivalence of reversing function.
hreverse :: [a] -> [a]
hreverse (x : xs) = reverse xs ++ [x]
hreverse [] = []

-- Constructing property for 'preverseCorrect'.
-- `haskEquiv'` will construct a property using `Arbitrary` instance.
propCorrect :: Property
propCorrect = haskEquiv' hreverse (preverseCorrect @PBuiltinList @PInteger)

propWrong :: Property
propWrong = haskEquiv' hreverse (preverseWrong @PBuiltinList @PInteger)

-- There is also `haskEquiv` which allows user to mannually specify
-- any generator they want. Generators should be provided in order of
-- input. The operators for making this list of Generator is imported
-- from `Generics.SOP`. (hint: it's using `NP`)
propCustom :: Property
propCustom =
    haskEquiv hreverse (TestableTerm preverseCorrect) (genList :* Nil)
  where
    genList :: Gen (TestableTerm (PBuiltinList PInteger))
    genList = do
        a <- vectorOf 10 arbitrary
        return $ pconstantT a

main :: IO ()
main = do
    -- This will fix some problems regarding text encoding.
    setLocaleEncoding utf8
    defaultMain . adjustOption go $
        testGroup "" [ testProperty "Correct 'preverse'" propCorrect
            , expectFail $ testProperty "Wrong 'preverse'" propWrong
            , testProperty "Correct 'preverse' with custom generator" propCustom
            ]
  where
    -- 100 test is way too small for property to search a counterexample,
    -- it is recommanded to use at least 10,000. However, more is the better.
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
