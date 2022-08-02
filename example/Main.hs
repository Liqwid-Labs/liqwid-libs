{-# LANGUAGE TemplateHaskell #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Example usage of 'plutarch-script-export'.

Example usage of 'plutarch-script-export'.
-}
module Main (main) where

import Data.Default (def)
import Data.Function ((&))
import Data.Text (Text)
import Development.GitRev (gitBranch, gitHash)
import Plutarch (ClosedTerm, pcon, plam, popaque)
import Plutarch.Api.V1 (PValidator)
import Plutarch.Prelude (PUnit (PUnit))
import ScriptExport.API (runServer)
import ScriptExport.Options (parseOptions)
import ScriptExport.ScriptInfo (mkValidatorInfo)
import ScriptExport.Types (Builders, insertBuilder)

main :: IO ()
main =
  parseOptions >>= runServer revision builders
  where
    -- This encodes the git revision of the server. It's useful for the caller
    -- to be able to ensure they are compatible with it.
    revision :: Text
    revision = $(gitBranch) <> "@" <> $(gitHash)

builders :: Builders
builders =
  def
    & insertBuilder @() "alwaysSucceeds" (\_ -> mkValidatorInfo alwaysSucceeds)

alwaysSucceeds :: ClosedTerm PValidator
alwaysSucceeds = plam $ \_ _ _ -> popaque (pcon PUnit)
