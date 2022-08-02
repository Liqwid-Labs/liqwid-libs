module Plutarch.Extra.Compile (mustCompile) where

import Data.Default (Default (Data))

-- | Compile a ClosedTerm, throwing an error if unsuccessful.
mustCompile :: ClosedTerm a -> Script
mustCompile t = case compile def t of
    Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
    Right s -> s
