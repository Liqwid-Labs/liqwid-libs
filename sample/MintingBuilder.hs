module MintingBuilder (specs) where

import Plutarch.Context (MintingBuilder (mbMintingCS), buildMinting, mint)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import qualified PlutusLedgerApi.V1.Value as Value

specs :: TestTree
specs =
    testGroup
        "Minting Builder Unit Tests"
        [ testCase "MintingBuilder succeeds with single input" $
            case buildMinting singleMint{mbMintingCS = Just "deadbeef"} of
                Left err -> assertFailure ("buildingMinting failed with error: " <> err)
                Right _ -> pure ()
        , testCase "MintingBuilder fails if currency symbol can't be found" $
            case buildMinting singleMint{mbMintingCS = Just "beefbeef"} of
                Left _ -> pure ()
                Right _ ->
                    assertFailure
                        ( "buildMinting should fail if an invalid CS is"
                            <> " passed, but it succeeded."
                        )
        , testCase "MintingBuilder fails with zero inputs" $
            case buildMinting mempty of
                Left _ -> pure ()
                Right _ ->
                    assertFailure
                        ( "buildMinting should fail when mbMintingCS,"
                            <> " but it passed."
                        )
        , testCase "MintingBuilder works with either of two Minting CS's" $
            case buildMinting doubleMint{mbMintingCS = Just "deadbeef"} of
                Left err -> assertFailure ("buildMinting failed with error " <> err)
                Right _ -> case buildMinting doubleMint{mbMintingCS = Just "bebe"} of
                    Left err -> assertFailure ("buildMinting failed with error " <> err)
                    Right _ -> pure ()
        ]

singleMint :: MintingBuilder
singleMint = mint (Value.singleton "deadbeef" "alivecow" 1)

doubleMint :: MintingBuilder
doubleMint = singleMint <> mint (Value.singleton "bebe" "smallcow" 1)
