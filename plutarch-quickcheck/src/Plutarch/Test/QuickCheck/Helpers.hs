{-# LANGUAGE RankNTypes #-}

module Plutarch.Test.QuickCheck.Helpers (loudEval) where

import Plutarch (ClosedTerm, Config (..), TracingMode (DoTracing))
import Plutarch.Evaluate (evalTerm)

loudEval :: ClosedTerm p -> ClosedTerm p
loudEval x =
  case evalTerm (Config {tracingMode = DoTracing}) x of
    Right (Right t, _, _) -> t
    Right (Left err, _, trace) -> error $ show err <> show trace -- TODO pretty this output
    Left err -> error $ show err
