{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Benchmark.Orphans () where

import UntypedPlutusCore qualified as UPLC
import Control.Parallel.Strategies (NFData)

deriving  newtype instance NFData UPLC.Size
