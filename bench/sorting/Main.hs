{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Default (def)
import Data.Tagged (Tagged (Tagged))
import Data.Text (unpack)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import PComparator (
    pcompareBy1,
    pcompareBy2,
    pcompareBy3,
    pequateBy1,
    pequateBy2,
    pequateBy3,
    pfromOrd1,
    pfromOrd2,
    pfromOrd3,
    pgeqBy1,
    pgeqBy2,
    pgeqBy3,
    pgreaterThanBy1,
    pgreaterThanBy2,
    pgreaterThanBy3,
    pleqBy1,
    pleqBy2,
    pleqBy3,
    plessThanBy1,
    plessThanBy2,
    plessThanBy3,
 )
import POrdering (POrdering' (PEQ', PGT'), pdot)
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
        , testGroup
            "POrdering"
            [ testGroup
                "PEq"
                [ singleTest "native" $ ScriptBench3 (plam (#==), pcon PEQ', pcon PGT')
                , singleTest "via PInner" $ ScriptBench3 (plam (#==), pconstant @PInteger 1, pconstant 2)
                ]
            , testGroup
                "PPartialOrd"
                [ singleTest "native (#<)" $ ScriptBench3 (plam (#<), pcon PEQ', pcon PGT')
                , singleTest "via PInner (#<)" $ ScriptBench3 (plam (#<), pconstant @PInteger 1, pconstant 2)
                , singleTest "native (#<=)" $ ScriptBench3 (plam (#<=), pcon PEQ', pcon PGT')
                , singleTest "via PInner (#<=)" $ ScriptBench3 (plam (#<=), pconstant @PInteger 1, pconstant 2)
                ]
            , testGroup
                "Semigroup"
                [ singleTest "native" $ ScriptBench3 (plam (<>), pcon PEQ', pcon PGT')
                , singleTest "via PInner" $ ScriptBench3 (pdot, pconstant @PInteger 1, pconstant 2)
                ]
            ]
        , testGroup
            "PComparator"
            [ testGroup
                "Size"
                [ singleTest "One function" (ScriptBench1 (pfromOrd1 @PInteger))
                , singleTest "Two functions" (ScriptBench1 (pfromOrd2 @PInteger))
                , singleTest "Three functions" (ScriptBench1 (pfromOrd3 @PInteger))
                ]
            , testGroup
                "pcompareBy"
                [ singleTest "One function" (ScriptBench4 (pcompareBy1, pfromOrd1 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Two functions" (ScriptBench4 (pcompareBy2, pfromOrd2 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Three functions" (ScriptBench4 (pcompareBy3, pfromOrd3 @PInteger, pconstant 1, pconstant 2))
                ]
            , testGroup
                "pequateBy"
                [ singleTest "One function" (ScriptBench4 (pequateBy1, pfromOrd1 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Two functions" (ScriptBench4 (pequateBy2, pfromOrd2 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Three functions" (ScriptBench4 (pequateBy3, pfromOrd3 @PInteger, pconstant 1, pconstant 2))
                ]
            , testGroup
                "pleqBy"
                [ singleTest "One function" (ScriptBench4 (pleqBy1, pfromOrd1 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Two functions" (ScriptBench4 (pleqBy2, pfromOrd2 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Three functions" (ScriptBench4 (pleqBy3, pfromOrd3 @PInteger, pconstant 1, pconstant 2))
                ]
            , testGroup
                "plessThanBy"
                [ singleTest "One function" (ScriptBench4 (plessThanBy1, pfromOrd1 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Two functions" (ScriptBench4 (plessThanBy2, pfromOrd2 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Three functions" (ScriptBench4 (plessThanBy3, pfromOrd3 @PInteger, pconstant 1, pconstant 2))
                ]
            , testGroup
                "pgeqBy"
                [ singleTest "One function" (ScriptBench4 (pgeqBy1, pfromOrd1 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Two functions" (ScriptBench4 (pgeqBy2, pfromOrd2 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Three functions" (ScriptBench4 (pgeqBy3, pfromOrd3 @PInteger, pconstant 1, pconstant 2))
                ]
            , testGroup
                "pgreaterThanBy"
                [ singleTest "One function" (ScriptBench4 (pgreaterThanBy1, pfromOrd1 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Two functions" (ScriptBench4 (pgreaterThanBy2, pfromOrd2 @PInteger, pconstant 1, pconstant 2))
                , singleTest "Three functions" (ScriptBench4 (pgreaterThanBy3, pfromOrd3 @PInteger, pconstant 1, pconstant 2))
                ]
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
        (ScriptBench2 (pnubSort @PInteger @PList, preplicate # plen # 1))

benchNub' :: (forall (s :: S). Term s PInteger) -> Int -> TestTree
benchNub' plen len =
    singleTest
        ("pnubSort, length " <> show len)
        (ScriptBench2 (pnubSort @PInteger @PBuiltinList, preplicate # plen # 1))

benchNubCustom :: (forall (s :: S). Term s PInteger) -> Int -> TestTree
benchNubCustom plen len =
    singleTest
        ("pnubSort, custom comparator, length " <> show len)
        ( ScriptBench3
            ( pnubSortBy @PInteger @PList
            , preverseComparator # pfromOrd
            , preplicate # plen # 1
            )
        )

-- Scaffolding and helpers

-- One argument
newtype ScriptBench1 (a :: S -> Type)
    = ScriptBench1 (ClosedTerm a)

instance (Typeable a) => IsTest (ScriptBench1 a) where
    run _ (ScriptBench1 t) _ =
        pure $ case evalBench t of
            Left err -> testFailed err
            Right (cpu, mem) -> testPassed . prettyPrintBenchResult cpu $ mem
    testOptions = Tagged []

-- Two arguments
newtype ScriptBench2 (a :: S -> Type) (b :: S -> Type)
    = ScriptBench2 (ClosedTerm (a :--> b), ClosedTerm a)

instance (Typeable a, Typeable b) => IsTest (ScriptBench2 a b) where
    run _ (ScriptBench2 (f, arg)) _ =
        pure . either testFailed testPassed $
            -- Normally we would use do-notation here, but as we're dealing with
            -- ClosedTerm (and thus, impredicative instantiation), and QuickLook can't
            -- deal with do-notation, we have to use manual binds.
            evalBenchTerm arg >>= \arg' ->
                evalBench (f # arg') >>= \(cpu, mem) ->
                    pure . prettyPrintBenchResult cpu $ mem
    testOptions = Tagged []

-- Three arguments
newtype ScriptBench3 (a :: S -> Type) (b :: S -> Type) (c :: S -> Type)
    = ScriptBench3 (ClosedTerm (a :--> b :--> c), ClosedTerm a, ClosedTerm b)

instance (Typeable a, Typeable b, Typeable c) => IsTest (ScriptBench3 a b c) where
    run _ (ScriptBench3 (f, arg1, arg2)) _ =
        pure . either testFailed testPassed $
            -- Same as above
            evalBenchTerm arg1 >>= \arg1' ->
                evalBenchTerm arg2 >>= \arg2' ->
                    evalBench (f # arg1' # arg2') >>= \(cpu, mem) ->
                        pure . prettyPrintBenchResult cpu $ mem
    testOptions = Tagged []

-- Four arguments
newtype ScriptBench4 (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (d :: S -> Type)
    = ScriptBench4 (ClosedTerm (a :--> b :--> c :--> d), ClosedTerm a, ClosedTerm b, ClosedTerm c)

instance (Typeable a, Typeable b, Typeable c, Typeable d) => IsTest (ScriptBench4 a b c d) where
    run _ (ScriptBench4 (f, arg1, arg2, arg3)) _ =
        pure . either testFailed testPassed $
            -- And again
            evalBenchTerm arg1 >>= \arg1' ->
                evalBenchTerm arg2 >>= \arg2' ->
                    evalBenchTerm arg3 >>= \arg3' ->
                        evalBench (f # arg1' # arg2' # arg3') >>= \(cpu, mem) ->
                            pure . prettyPrintBenchResult cpu $ mem
    testOptions = Tagged []

evalBenchTerm ::
    forall (a :: S -> Type).
    ClosedTerm a ->
    Either String (ClosedTerm a)
evalBenchTerm t = case evalTerm def t of
    Left err -> Left $ "Argument unexpectedly failed to compile: \n" <> unpack err
    Right (res, _, _) -> case res of
        Left err -> Left $ "Argument unexpectedly failed to evaluate: \n" <> show err
        Right t' -> pure t'

evalBench ::
    forall (a :: S -> Type).
    ClosedTerm a ->
    Either String (CostingInteger, CostingInteger)
evalBench t = case compile def t of
    Left err -> Left $ "Function unexpectedly failed to compile: \n" <> unpack err
    Right script -> case evalScriptHuge script of
        (_, ExBudget (ExCPU cpu) (ExMemory mem), _) -> pure (cpu, mem)

prettyPrintBenchResult :: CostingInteger -> CostingInteger -> String
prettyPrintBenchResult cpu mem =
    "CPU used: "
        <> commaSepThousands cpu
        <> "\nMemory used: "
        <> commaSepThousands mem
        <> "\n"

commaSepThousands :: CostingInteger -> String
commaSepThousands i = case i `quotRem` 1000 of
    (0, r) -> show r
    (d, r) -> commaSepThousands d <> "," <> ensureThreeDigits r

ensureThreeDigits :: CostingInteger -> String
ensureThreeDigits i
    | i < 10 = "00" <> show i
    | i < 100 = "0" <> show i
    | otherwise = show i
