module Main (main) where

import Control.Monad (replicateM)
import Data.Text (pack)
import System.Random.Stateful (runStateGen, uniformRM)
import Test.Benchmark.Common (ImplData (ImplData, val), multiImplData)
import Test.Benchmark.Cost (meanVal, rankOnPerAxisStat, writeComparisonPerAxisCSVs, writePerAxisCSVs)
import Test.Benchmark.Main (benchMain)
import Test.Benchmark.Plutarch (mkTermImplMetaData, sampleTerm)
import Test.Benchmark.Plutus (statsByAxis)
import Test.Benchmark.Sized (
  Cardinality (Cardinality),
  SUniversalGen (SUniversalGen),
  benchSizesUniversal,
 )

-- | Waste budget proportional to the argument
pwaste ::
  forall (s :: S).
  Term s (PInteger :--> PUnit)
pwaste = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif (n #== 0) (pcon PUnit) (self #$ n - 1)

-- | Waste budget proportional to the argument, extra CPU-heavy
pwasteCPU ::
  forall (s :: S).
  Term s (PInteger :--> PUnit)
pwasteCPU = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      (pcon PUnit)
      (plet (str #== str) $ \_ -> self #$ n - 1)
  where
    str = pconstant (pack $ replicate 1000 'a') :: ClosedTerm PString

main :: IO ()
main = benchMain $ \dir -> do
  print $ mkTermImplMetaData "pwasteCPU" pwasteCPU
  print $ sampleTerm $ pwasteCPU # 10_000_000_000
  print $ mkTermImplMetaData "pwaste" pwaste
  print $ sampleTerm $ pwaste # 10_000_000_000

  let gen =
        SUniversalGen @[Integer]
          (\size -> Cardinality $ 10 ^ (fromIntegral size :: Int))
          (\size -> replicateM size [0 .. 9])
          (\size -> flip runStateGen (replicateM size . uniformRM (0, 9)))
  stats1 <-
    ImplData "find 3" . statsByAxis
      <$> benchSizesUniversal
        gen
        (\list -> sampleTerm $ pfind # plam (#== 3) # pconstant list)
        10000
        [0 .. 10]
  writePerAxisCSVs dir stats1

  stats2 <-
    ImplData "find 4" . statsByAxis
      <$> benchSizesUniversal
        gen
        (\list -> sampleTerm $ pfind # plam (#== 4) # pconstant list)
        10000
        [0 .. 10]
  writePerAxisCSVs dir stats2

  let multi = multiImplData "find" [stats1, stats2]
      comp = rankOnPerAxisStat "mean" (.meanVal) multi

  writeComparisonPerAxisCSVs dir comp
