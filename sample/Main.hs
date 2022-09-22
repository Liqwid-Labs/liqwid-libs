{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Api.V1 (PValidator)
import Plutarch.Extra.TermCont (ptryFromC)
import Plutarch.Prelude (PAsData, PInteger, Term, pfromData, pif, plam, popaque, ptraceError, unTermCont, (#==))
import Plutarch.Test.Precompiled (fromPTerm, (@!>), (@&), (@>))
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified PlutusTx as PlutusTx

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $
        testGroup
            "test suite"
            [ sampleValidatorTest
            ]

sampleValidator :: Term s PValidator
sampleValidator = plam $ \_ x _ -> unTermCont $ do
    (pfromData -> x', _) <- ptryFromC @(PAsData PInteger) x
    pure $
        popaque $
            pif
                (x' #== 10)
                (ptraceError "x shouldn't be 10")
                (x')

sampleValidatorTest :: TestTree
sampleValidatorTest = fromPTerm "sample validator" sampleValidator $ do
    [PlutusTx.toData (), PlutusTx.toData (1 :: Integer), PlutusTx.toData ()] @> "It should succeed when given 1"
    [PlutusTx.toData (), PlutusTx.toData (10 :: Integer), PlutusTx.toData ()] @!> "It should fail when given 10"
    [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()] @> "It should fail when given non integer"

    [PlutusTx.toData ()] @& do
        [PlutusTx.toData (1 :: Integer), PlutusTx.toData ()] @> "(Sharing first argument) It should succeed when given 1"
        [PlutusTx.toData (10 :: Integer), PlutusTx.toData ()] @!> "(Sharing first argument) It should fail when given 10"
        [PlutusTx.toData (), PlutusTx.toData ()] @!> "(Sharing first argument) It should fail when given non integer"
