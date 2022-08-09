module MintingBuilder (specs) where

import Plutarch.Context (
    MintingBuilder,
    mint,
    tryBuildMinting,
    withMinting,
 )
import PlutusLedgerApi.V2 (singleton)
import qualified Prettyprinter as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

specs :: TestTree
specs =
    testGroup
        "Minting Builder Unit Tests"
        [ testCase "MintingBuilder succeeds with single input" $
            case tryBuildMinting mempty $ singleMint <> withMinting "deadbeef" of
                Left err -> assertFailure ("buildingMinting failed with error: " <> show (P.pretty err))
                Right _ -> pure ()
        , testCase "MintingBuilder fails if currency symbol can't be found" $
            case tryBuildMinting mempty $ singleMint <> withMinting "beefbeef" of
                Left _ -> pure ()
                Right _ ->
                    assertFailure
                        ( "tryBuildMinting mempty should fail invalid CS is"
                            <> " passed, but it succeeded."
                        )
        , testCase "MintingBuilder fails with unspecified currency symbol" $
            case tryBuildMinting mempty mempty of
                Left _ -> pure ()
                Right _ ->
                    assertFailure
                        ( "tryBuildMinting mempty should fail when mbMintingCS,"
                            <> " but it passed."
                        )
        , testCase "MintingBuilder works with either of two Minting CS's" $
            case tryBuildMinting mempty $ doubleMint <> withMinting "deadbeef" of
                Left err -> assertFailure ("tryBuildMinting mempty failed with error " <> show (P.pretty err))
                Right _ -> case tryBuildMinting mempty $ doubleMint <> withMinting "bebe" of
                    Left err -> assertFailure ("tryBuildMinting mempty failed with error " <> show (P.pretty err))
                    Right _ -> pure ()
        ]

singleMint :: MintingBuilder
singleMint = mint (singleton "deadbeef" "alivecow" 1)

doubleMint :: MintingBuilder
doubleMint = singleMint <> mint (singleton "bebe" "smallcow" 1)
