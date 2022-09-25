{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.Compile (mustCompile) where

import qualified Data.Text as T
import Plutarch (
  Config (Config, tracingMode),
  TracingMode (DetTracing),
  compile,
 )
import PlutusLedgerApi.V2 (Script)

{- | Compile a ClosedTerm, throwing an error if unsuccessful.

     @since 2.0.0
-}
mustCompile :: forall (a :: S -> Type). ClosedTerm a -> Script
mustCompile t = case compile conf t of
  Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
  Right s -> s
  where
    conf = Config {tracingMode = DetTracing}
