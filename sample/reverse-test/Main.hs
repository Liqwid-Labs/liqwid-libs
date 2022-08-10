
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
import Test.QuickCheck (
    NonNegative (NonNegative),
    arbitrary,
    getNonNegative,
    shrink,
 )
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.QuickCheck

preverseCorrect ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    PIsListLike t a =>
    Term s (t a :--> t a)
preverseCorrect = phoistAcyclic $ pfoldl # plam (\ys y -> pcons # y # ys) # pnil

preverseWrong ::
    forall (t :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
    PIsListLike t a =>
    Term s (t a :--> t a)
preverseWrong = plam id

hreverse :: [Integer] -> [Integer]
hreverse (x : xs) = reverse xs ++ [x]
hreverse [] = []

propCorrect :: Property
propCorrect = haskEquiv' hreverse (preverseCorrect @PBuiltinList)

propWrong :: Property
propWrong = haskEquiv' hreverse (preverseWrong @PBuiltinList)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . adjustOption go $
        testGroup "" [ testProperty "Correct 'preverse'" propCorrect
            , expectFail $ testProperty "Wrong 'preverse'" propWrong
            ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000
