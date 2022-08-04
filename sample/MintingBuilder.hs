module MintingBuilder (specs) where

import Plutarch.Context
    ( MintingBuilder, buildMinting, mint, withMinting )
import PlutusLedgerApi.V2 ( singleton )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( assertFailure, testCase )

specs :: TestTree
specs =
    testGroup
        "Minting Builder Unit Tests"
        [ testCase "MintingBuilder succeeds with single input" $
            case buildMinting $ singleMint <> withMinting "deadbeef" of
                Left err -> assertFailure ("buildingMinting failed with error: " <> err)
                Right _ -> pure ()
        , testCase "MintingBuilder fails if currency symbol can't be found" $
            case buildMinting $ singleMint <> withMinting "beefbeef" of
                Left _ -> pure ()
                Right _ ->
                    assertFailure
                        ( "buildMinting should fail invalid CS is"
                            <> " passed, but it succeeded."
                        )
        , testCase "MintingBuilder fails with unspecified currency symbol" $
            case buildMinting mempty of
                Left _ -> pure ()
                Right _ ->
                    assertFailure
                        ( "buildMinting should fail when mbMintingCS,"
                            <> " but it passed."
                        )
        , testCase "MintingBuilder works with either of two Minting CS's" $
            case buildMinting $ doubleMint <> withMinting "deadbeef" of
                Left err -> assertFailure ("buildMinting failed with error " <> err)
                Right _ -> case buildMinting $ doubleMint <> withMinting "bebe" of
                    Left err -> assertFailure ("buildMinting failed with error " <> err)
                    Right _ -> pure ()
        ]

singleMint :: MintingBuilder
singleMint = mint (singleton "deadbeef" "alivecow" 1)

doubleMint :: MintingBuilder
doubleMint = singleMint <> mint (singleton "bebe" "smallcow" 1)
