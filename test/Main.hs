{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

import qualified FunctorProp
import qualified NumericProp
import qualified OrdProp
import qualified TraversableProp

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . testGroup "Liqwid Plutarch Extra Tests" $
    [ OrdProp.tests
    , NumericProp.tests
    , TraversableProp.tests
    , FunctorProp.tests
    ]
