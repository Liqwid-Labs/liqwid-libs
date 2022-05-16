{-# LANGUAGE TypeApplications #-}

{- | Module: Main
 Copyright: (C) Liqwid Labs 2022
 License: Apache 2.0
 Portability: GHC only
 Stability: Experimental

 Example of @plutarch-quickcheck@ tests. These are meant to be read as source
 code.
-}
module Main (main) where

import Data.Tagged (Tagged (Tagged))
import Data.Universe (Finite (cardinality, universeF), Universe (universe))
import Plutarch
import Plutarch.Integer (PInteger, pquot)
import Plutarch.Maybe
--import Plutarch.TermCont
--import Plutarch.Trace

import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutarch.Property (classifiedProperty)
import Test.Tasty.QuickCheck (testProperty)

data HandlerCases = EvenNumber | OddNumber
    deriving stock (Eq, Show)

instance Universe HandlerCases where
    universe = [EvenNumber, OddNumber]

instance Finite HandlerCases where
    universeF = universe
    cardinality = Tagged 2

classifier :: Integer -> HandlerCases
classifier a
  | even a = EvenNumber
  | otherwise = OddNumber

generator :: HandlerCases -> Gen Integer
generator EvenNumber = (arbitrary :: Gen Integer) >>= (\number -> return $ number * 2)
generator OddNumber = (arbitrary :: Gen Integer) >>= (\number -> return $ number * 2 + 1)
  
shrinker :: Integer -> [Integer]
shrinker = shrink

expected :: forall (s :: S). Term s ( PInteger :--> PMaybe PInteger)
expected = phoistAcyclic $
  plam $ \_t -> unTermCont $ do
    pure $ pcon $ PNothing

definition :: forall (s :: S). Term s (PInteger :--> PInteger)
definition = pquot # 2 

ourProperty :: Property
ourProperty = classifiedProperty generator shrinker expected classifier definition

main :: IO ()
main = do
    defaultMain . testGroup "Handlers" $
        [ testProperty "will Fail" ourProperty
        ]
