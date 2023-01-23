{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

import FunctorProp qualified
import NumericProp qualified
import OrdProp qualified
import TraversableProp qualified
import ValueProp qualified

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . testGroup "Liqwid Plutarch Extra Tests" $
    [ OrdProp.tests
    , NumericProp.tests
    , TraversableProp.tests
    , FunctorProp.tests
    , ValueProp.tests
    ]
