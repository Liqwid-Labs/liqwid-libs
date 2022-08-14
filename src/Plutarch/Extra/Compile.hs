{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.Compile (mustCompile) where

import qualified Data.Text as T
import Plutarch (ClosedTerm, Config (..), TracingMode (DetTracing), compile)
import PlutusLedgerApi.V1 (Script)

{- | Compile a ClosedTerm, throwing an error if unsuccessful.

     @since 2.0.0
-}
mustCompile :: ClosedTerm a -> Script
mustCompile t = case compile conf t of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right s -> s
  where
    conf = Config{tracingMode = DetTracing}
