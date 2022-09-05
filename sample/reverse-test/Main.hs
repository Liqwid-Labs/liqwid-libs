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
import Plutarch.Prelude (
  PBuiltinList,
  PInteger,
  PIsListLike,
  S,
  Term,
  Type,
  pcons,
  pfoldl,
  phoistAcyclic,
  plam,
  pnil,
  (#),
  (:-->),
 )

import Plutarch.Test.QuickCheck (
  Equality (OnPEq),
  Partiality (ByComplete),
  TestableTerm (TestableTerm),
  haskEquiv,
  haskEquiv',
  pconstantT,
 )

import Generics.SOP (NP (Nil, (:*)))
import Test.QuickCheck (
  arbitrary,
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  QuickCheckTests,
  testProperty,
  vectorOf,
 )

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
hreverse :: forall {a :: Type}. [a] -> [a]
hreverse (x : xs) = reverse xs ++ [x]
hreverse [] = []

-- Constructing property for 'preverseCorrect'.
-- `haskEquiv'` will construct a property using `Arbitrary` instance.
propCorrect :: Property
propCorrect = haskEquiv' @( 'OnPEq) @( 'ByComplete) hreverse (preverseCorrect @PBuiltinList @PInteger)

propWrong :: Property
propWrong = haskEquiv' @( 'OnPEq) @( 'ByComplete) hreverse (preverseWrong @PBuiltinList @PInteger)

-- There is also `haskEquiv` which allows user to mannually specify
-- any generator they want. Generators should be provided in order of
-- input. The operators for making this list of Generator is imported
-- from `Generics.SOP`. (hint: it's using `NP`)
propCustom :: Property
propCustom =
  haskEquiv @( 'OnPEq) @( 'ByComplete) hreverse (TestableTerm preverseCorrect) (genList :* Nil)
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
    testGroup
      ""
      [ testProperty "Correct 'preverse'" propCorrect
      , expectFail $ testProperty "Wrong 'preverse'" propWrong
      , testProperty "Correct 'preverse' with custom generator" propCustom
      ]
  where
    -- 100 tests is way too small for a property test to search for a counterexample,
    -- it is recommanded to use at least 10,000. However, more is better.
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
