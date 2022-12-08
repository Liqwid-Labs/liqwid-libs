module Plutarch.Extra.Script (applyArguments) where

import Control.Lens (over)
import Plutarch.Script (Script (Script))
import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import UntypedPlutusCore qualified as UPLC

{- | Applys 'Data' to Script

 @since 3.20.0
-}
applyArguments :: Script -> [PLC.Data] -> Script
applyArguments (Script p) args =
  let termArgs = fmap (PLC.mkConstant ()) args
      applied t = PLC.mkIterApp () t termArgs
   in Script $ over UPLC.progTerm applied p
