{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.Compile (mustCompile) where

import Data.Default (Default (def))
import qualified Data.Text as T
import Plutarch (compile)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (Script)
import qualified Data.Text as T
import Plutarch (Config (..), TracingMode (DetTracing), compile)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (Script)

{- | Compile a ClosedTerm, throwing an error if unsuccessful.

     @since 2.0.0
-}
mustCompile :: ClosedTerm a -> Script
mustCompile t = case compile conf t of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right s -> s
  where
    conf = Config{tracingMode = DetTracing}
