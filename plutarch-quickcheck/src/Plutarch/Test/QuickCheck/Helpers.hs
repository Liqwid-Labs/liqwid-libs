{-# LANGUAGE RankNTypes #-}

module Plutarch.Test.QuickCheck.Helpers (loudEval) where

import Plutarch (ClosedTerm, Config (Tracing), 
  LogLevel (LogInfo), TracingMode (DoTracing))
import Plutarch.Evaluate (evalTerm)

loudEval :: ClosedTerm p -> ClosedTerm p
loudEval x =
  case evalTerm (Tracing LogInfo DoTracing) x of
    Right (Right t, _, _) -> t
    Right (Left err, _, trace) -> error $ show err <> show trace -- TODO pretty this output
    Left err -> error $ show err
