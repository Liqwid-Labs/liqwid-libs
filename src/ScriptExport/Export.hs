{-# LANGUAGE TemplateHaskell #-}

module ScriptExport.Export (
  exportMain,
) where

import ScriptExport.API (runServer)
import ScriptExport.File (runFile)
import ScriptExport.Options (Options (FileOption, ServerOption), parseOptions)
import ScriptExport.Types (Builders)

import Data.Text (Text)
import Development.GitRev (gitBranch, gitHash)

{- | Program entry point. It provides both file and servant api.

 @since 2.0.0
-}
exportMain :: Builders -> IO ()
exportMain builders = do
  opts <- parseOptions
  case opts of
    ServerOption so -> runServer revision builders so
    FileOption fo -> runFile revision builders fo
  where
    -- This encodes the git revision of the server. It's useful for the caller
    -- to be able to ensure they are compatible with it.
    revision :: Text
    revision = $(gitBranch) <> "@" <> $(gitHash)
