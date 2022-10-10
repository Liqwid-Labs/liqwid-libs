{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Example usage of 'plutarch-script-export'.

Example usage of 'plutarch-script-export'.
-}
module Main (main) where

import Data.Default (def)
import Data.Function ((&))
import Plutarch.Api.V2 (PValidator)
import Plutarch.Prelude (ClosedTerm, PUnit (PUnit), pcon, plam, popaque)
import ScriptExport.Export (exportMain)
import ScriptExport.ScriptInfo (mkValidatorInfo)
import ScriptExport.Types (Builders, insertBuilder)

main :: IO ()
main = exportMain builders

builders :: Builders
builders =
  def
    & insertBuilder @() "alwaysSucceeds" (\_ -> mkValidatorInfo alwaysSucceeds)

alwaysSucceeds :: ClosedTerm PValidator
alwaysSucceeds = plam $ \_ _ _ -> popaque (pcon PUnit)
