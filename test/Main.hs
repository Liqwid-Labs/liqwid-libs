{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, testGroup)

import qualified Properties.OrdProp as OrdProp
import qualified Properties.NumericProp as NumericProp
import qualified Properties.TraversableProp as TraversableProp

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . testGroup "Liqwid Plutarch Extra Tests" $
    [ OrdProp.tests
    , NumericProp.tests
    , TraversableProp.tests
    ]
