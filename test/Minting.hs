{-# LANGUAGE ViewPatterns #-}

module Minting (mintingBuilderProperty) where

import BaseBuilder (
    BuilderInterface (Mint),
    baseRules,
    build,
    genAnyValue,
    genCurrencySymbol,
    genValueWithCS,
 )
import Control.Monad (replicateM)
import Data.Maybe (isNothing)
import Plutarch.Context (buildMinting, defaultConfig)
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value, symbols)
import Test.Tasty.QuickCheck (
    Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    Testable (property),
    checkCoverage,
    cover,
    elements,
    forAllShrink,
    listOf1,
 )

data ExpectedResult
    = ShouldBuild
    | Shouldn'tBuild
    deriving stock (Show, Eq)

genCases :: Gen ([BuilderInterface], ExpectedResult)
genCases = do
    infs <- arbitrary
    cs <- genCurrencySymbol
    homogeneousMints <- listOf1 (genValueWithCS cs) >>= return . fmap Mint

    -- Value must contain atleast two values two be heterogeneous
    let hm = listOf1 genAnyValue >>= return . fmap Mint
    heterogeneousMints <- mconcat <$> replicateM 2 hm

    let mintDropped = filter (\case Mint _ -> False; _ -> True) infs

    elements
        [ (mintDropped <> homogeneousMints, ShouldBuild)
        , (mintDropped <> heterogeneousMints, Shouldn'tBuild)
        ]

uniformCurrencySymbol ::
    Value ->
    Maybe CurrencySymbol
uniformCurrencySymbol (symbols -> xs)
    | length xs == 1 = Just $ head xs
    | otherwise = Nothing

shrinkCases ::
    ([BuilderInterface], ExpectedResult) ->
    [([BuilderInterface], ExpectedResult)]
shrinkCases (infs, _) = (\x -> (x, expect x)) <$> shrink infs
  where
    f (Mint v) = v
    f _ = mempty
    expect xs =
        case uniformCurrencySymbol (mconcat $ f <$> xs) of
            Just _ -> ShouldBuild
            Nothing -> Shouldn'tBuild

mintingBuilderProperty :: Property
mintingBuilderProperty = forAllShrink genCases shrinkCases go
  where
    go (infs, tag) = checkCoverage $
        cover 50.0 (tag == Shouldn'tBuild) "shouldn't pass" $
            cover 50.0 (tag == ShouldBuild) "should pass" $
                case tag of
                    ShouldBuild ->
                        case ctx of
                            Just sc -> baseRules sc infs
                            Nothing -> property False
                    Shouldn'tBuild -> property $ isNothing ctx
      where
        ctx = buildMinting defaultConfig $ build infs
