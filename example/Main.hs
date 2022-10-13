{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Example usage of 'plutarch-script-export'.

Example usage of 'plutarch-script-export'.
-}
module Main (main) where

import Data.Default (def)
import Data.Function ((&))
import Data.Map (fromList)
import Plutarch.Api.V2 (PValidator, mkValidator)
import Plutarch.Prelude (ClosedTerm, PUnit (PUnit), pcon, plam, popaque)
import PlutusLedgerApi.V2 (Validator (getValidator))
import ScriptExport.Export (exportMain)
import ScriptExport.ScriptInfo (ScriptExport (ScriptExport), mkValidatorInfo)
import ScriptExport.Types (
  Builders,
  insertBuilder,
  insertScriptExport,
  insertStaticBuilder,
 )

main :: IO ()
main = exportMain builders

builders :: Builders
builders =
  def
    & insertStaticBuilder "alwaysSucceeds" (mkValidatorInfo alwaysSucceeds)
    & insertBuilder @() "alwaysSucceedsParam" (const $ mkValidatorInfo alwaysSucceeds)
    & insertScriptExport "my-onchain-project" project

alwaysSucceeds :: ClosedTerm PValidator
alwaysSucceeds = plam $ \_ _ _ -> popaque (pcon PUnit)

project :: ScriptExport Int
project =
  ScriptExport
    "1.2.3"
    ( fromList
        [ ("alwaysSucceeds", getValidator $ mkValidator def alwaysSucceeds)
        ]
    )
    10
