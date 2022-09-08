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
import Plutarch.Internal.PlutusType (PlutusType (pcon', pmatch'))
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
        ]
  where
    -- Need to do it this way because there's no Enum instance for Term s
    -- PInteger
    lengths :: [(forall (s :: S). Term s PInteger, Int)]
    lengths = [(pconstant i, fromIntegral i) | i <- [1 .. 40]]

-- Benchmarks

-- Implementation of <> for the PInner for POrdering'
pdot :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdot = phoistAcyclic $
    plam $ \x y ->
        pif (x #< 2) (x * y) x

-- Direct implementation of POrdering rep for benching against
data POrdering' (s :: S)
    = PLT'
    | PEQ'
    | PGT'
    deriving stock (Show, Generic)
    deriving anyclass (PEq, PShow, PPartialOrd, POrd)

instance PlutusType POrdering' where
    type PInner POrdering' = PInteger
    pcon' = \case
        PLT' -> 0
        PEQ' -> 1
        PGT' -> 2
    pmatch' x f =
        pif (x #== 0) (f PLT') (pif (x #== 1) (f PEQ') (f PGT'))

{-
instance PEq POrdering' where
    x #== y = pmatch x $ \case
        PLT' -> pmatch y $ \case
            PLT' -> pcon PTrue
            _ -> pcon PFalse
        PEQ' -> pmatch y $ \case
            PEQ' -> pcon PTrue
            _ -> pcon PFalse
        PGT' -> pmatch y $ \case
            PGT' -> pcon PTrue
            _ -> pcon PFalse
-}

-- | @since 3.6.0
instance Semigroup (Term s POrdering') where
    x <> y = pmatch x $ \case
        PLT' -> pcon PLT'
        PEQ' -> y
        PGT' -> pcon PGT'

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
