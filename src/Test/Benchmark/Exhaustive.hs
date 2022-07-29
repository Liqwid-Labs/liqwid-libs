{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Helpers for making exhaustive generators.
module Test.Benchmark.Exhaustive (exhGenBags, exhGenSets, cardSets) where

import Acc (Acc, fromReverseList)
import Control.Monad (join)
import GHC.Exts (IsList (toList))
import Numeric.SpecFunctions (choose)
import Test.Benchmark.Sized (Cardinality (..))

{- | Exhaustively generate lists (bags) of a given size.

 No list is generated twice.
 Duplicate elements are allowed.
 Output list elements are ordered like in the input list of unique elements.
 Thus, duplicate elements are always grouped in a contiguous sublist.
-}
exhGenBags ::
  -- | number of elems to be generated
  Int ->
  -- | choice of unique elements (a set)
  [a] ->
  [[a]]
exhGenBags num choice = map toList $ go num choice
  where
    go :: Int -> [a] -> [Acc a]
    go num [x] = pure . fromReverseList $ replicate num x
    go 0 _ = pure []
    go num (x : xs) = do
      reps <- [num, num - 1 .. 0]
      (fromReverseList (replicate reps x) <>) <$> go (num - reps) xs
    go _ [] = error "no choice"

-- | Exhaustively generate lists (subsets) of a given size.
exhGenSets ::
  -- | number of elems to be generated
  Int ->
  -- | choice of unique elements (the superset)
  [a] ->
  [[a]]
exhGenSets num choice = go num (length choice) choice
  where
    go 0 _ _ = pure []
    go n c cs@(x : xs)
      | n == c = pure cs
      | n > c = err
      | otherwise = do
          join
            [ (x :) <$> go (n - 1) (c - 1) xs
            , go n (c - 1) xs
            ]
    go _ _ [] = err
    err = error "requested more set elements than available"

{- | Cardinality of list of subsets, given set size and size of superset

 Use in conjunction with 'exhGenSets'.
-}
cardSets :: Int -> Int -> Cardinality
cardSets n k =
  -- 'choose' from 'math-functions' is accurate to 12 decimal places
  if c > 10e12 then HugeCardinality else Cardinality $ round c
  where
    c = choose n k
