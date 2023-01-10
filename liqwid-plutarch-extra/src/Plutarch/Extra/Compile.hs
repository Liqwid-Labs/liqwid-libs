{-# LANGUAGE RankNTypes #-}

module Plutarch.Extra.Compile (
  mustCompile,
  mustCompileTracing,
) where

import Data.Text qualified as T
import Plutarch (
  Config (Config, tracingMode),
  Script,
  TracingMode (DetTracing),
  compile,
 )

{- | Compile a 'ClosedTerm', throwing an error if unsuccessful.

     @since 2.0.0
-}
mustCompile :: forall (a :: S -> Type). ClosedTerm a -> Script
mustCompile t = case compile conf t of
  Left err -> error $ unwords ["Plutarch compilation error:", T.unpack err]
  Right s -> s
  where
    conf = Config {tracingMode = DetTracing}

-- Like 'mustCompile', but with tracing turned on.
--
-- @since 3.8.0
mustCompileTracing ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Script
mustCompileTracing term =
  case compile Config {tracingMode = DetTracing} term of
    Left err ->
      error $
        unwords
          [ "Plutarch compilation error: "
          , T.unpack err
          ]
    Right script -> script
