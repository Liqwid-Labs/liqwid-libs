{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Benchmark.Orphans () where

import Control.Parallel.Strategies (NFData)
import UntypedPlutusCore qualified as UPLC

deriving newtype instance NFData UPLC.Size
