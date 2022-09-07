{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Default (def)
import Data.Tagged (Tagged (Tagged))
import Data.Text (unpack)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (compile)
import Plutarch.Evaluate (evalScriptHuge, evalTerm)
import Plutarch.Extra.List (preplicate)
import Plutarch.Extra.Ord (pfromOrd, pnubSort, pnubSortBy, preverseComparator)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExMemory (
    CostingInteger,
    ExCPU (ExCPU),
    ExMemory (ExMemory),
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Providers (
    IsTest (run, testOptions),
    singleTest,
    testFailed,
    testPassed,
 )
import Type.Reflection (Typeable)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain . testGroup "Sorting-related benchmarks" $
        [ testGroup
            "nub"
            [ testGroup "PList PInteger" $ uncurry benchNub <$> lengths
            , testGroup "PList PInteger, custom comparator" $ uncurry benchNubCustom <$> lengths
            , testGroup "PBuiltinList PInteger" $ uncurry benchNub' <$> lengths
            ]
        ]
  where
    -- Need to do it this way because there's no Enum instance for Term s
    -- PInteger
    lengths :: [(forall (s :: S). Term s PInteger, Int)]
    lengths = [(pconstant i, fromIntegral i) | i <- [1 .. 40]]

-- Benchmarks

benchNub :: (forall (s :: S). Term s PInteger) -> Int -> TestTree
benchNub plen len =
    singleTest
        ("pnubSort, length " <> show len)
        (ScriptBench (pnubSort @PInteger @PList, preplicate # plen # 1))

benchNub' :: (forall (s :: S). Term s PInteger) -> Int -> TestTree
benchNub' plen len =
    singleTest
        ("pnubSort, length " <> show len)
        (ScriptBench (pnubSort @PInteger @PBuiltinList, preplicate # plen # 1))

benchNubCustom :: (forall (s :: S). Term s PInteger) -> Int -> TestTree
benchNubCustom plen len =
    singleTest
        ("pnubSort, custom comparator, length " <> show len)
        ( ScriptBench
            ( pnubSortBy @PInteger @PList # (preverseComparator # pfromOrd)
            , preplicate # plen # 1
            )
        )

-- Scaffolding and helpers

newtype ScriptBench (a :: S -> Type) (b :: S -> Type)
    = ScriptBench (ClosedTerm (b :--> a), ClosedTerm b)

instance (Typeable a, Typeable b) => IsTest (ScriptBench a b) where
    run _ (ScriptBench (f, arg)) _ = pure $ case evalTerm def arg of
        Left err -> testFailed $ "Argument unexpectedly failed to compile: \n" <> unpack err
        Right (res, _, _) -> case res of
            Left err -> testFailed $ "Argument unexpectedly failed to evaluate: \n" <> show err
            Right arg' -> case compile def (f # arg') of
                Left err -> testFailed $ "Function unexpectedly failed to compile: \n" <> show err
                Right script -> case evalScriptHuge script of
                    (_, ExBudget (ExCPU cpu) (ExMemory mem), _) ->
                        testPassed $
                            "CPU used: "
                                <> commaSepThousands cpu
                                <> "\nMemory used: "
                                <> commaSepThousands mem
                                <> "\n"
    testOptions = Tagged []

commaSepThousands :: CostingInteger -> String
commaSepThousands i = case i `quotRem` 1000 of
    (0, r) -> show r
    (d, r) -> commaSepThousands d <> "," <> ensureThreeDigits r

ensureThreeDigits :: CostingInteger -> String
ensureThreeDigits i
    | i < 10 = "00" <> show i
    | i < 100 = "0" <> show i
    | otherwise = show i
