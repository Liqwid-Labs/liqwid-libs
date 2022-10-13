{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Example usage of 'plutarch-script-export'.

Example usage of 'plutarch-script-export'.
-}
module Main (main) where

import Data.Default (def)
import Data.Function ((&))
import Data.Map (fromList)
import Data.Text (unpack)
import Plutarch.Api.V2 (PValidator, mkValidator)
import Plutarch.Prelude (
  ClosedTerm,
  PInteger,
  PUnit (PUnit),
  pcon,
  pconstant,
  plam,
  plet,
  popaque,
  tcont,
  unTermCont,
  (#),
  (:-->),
 )
import PlutusLedgerApi.V2 (Validator (getValidator))
import Ply (ScriptRole (ValidatorRole), toScript, (#))
import Ply.Plutarch.TypedWriter (mkEnvelope)
import ScriptExport.Export (exportMain)
import ScriptExport.ScriptInfo (
  Linker,
  RawScriptExport (RawScriptExport),
  ScriptExport (ScriptExport),
  exportParam,
  fetchTS,
  mkValidatorInfo,
 )
import ScriptExport.Types (
  Builders,
  insertBuilder,
  insertScriptExportWithLinker,
  insertStaticBuilder,
 )

main :: IO ()
main = exportMain builders

{-
This is the collection of builders. API and file will be created based on provided
builders. There are various `insertXYZBuilder` functions to provide versatile ways
of adding builders.

`insertStaticBuilder` will insert builder that does not have any
argument--as "static" suggests.

`insertBuilder` will insert builder from a function. Argument type
needs `Aeson.FromJSON` and return type needs `Aeson.ToJSON`.

`insertScriptExportWithLinker` is similar to `insertBuilder` but
specialized to `RawScriptExport` and `Linker`. It will automatically
handle linker given parameter. Also, it will return serialized
`RawScript` if no parameter is given.
-}
builders :: Builders
builders =
  def
    & insertStaticBuilder "alwaysSucceeds" (mkValidatorInfo alwaysSucceeds)
    & insertBuilder @Integer
      "alwaysSucceedsParam"
      (\x -> mkValidatorInfo (alwaysSucceedsParam Plutarch.Prelude.# pconstant x))
    & insertStaticBuilder "my-onchain-project" myproject
    & insertScriptExportWithLinker "my-onchain-project-param" myProjectParameterized myProjectLinker

-- This is our dummy validator.
alwaysSucceeds :: ClosedTerm PValidator
alwaysSucceeds = plam $ \_ _ _ -> popaque (pcon PUnit)

-- This is our dummy paramterized validator.
alwaysSucceedsParam :: ClosedTerm (PInteger :--> PValidator)
alwaysSucceedsParam = plam $ \x _ _ _ -> unTermCont $ do
  _ <- tcont $ plet $ x + x
  pure $ popaque (pcon PUnit)

-- This is example `ScriptExport`
myproject :: ScriptExport Int
myproject =
  ScriptExport
    "1.2.3"
    ( fromList
        [ ("alwaysSucceeds", getValidator $ mkValidator def alwaysSucceeds)
        ]
    )
    10

-- This is example `RawScriptExport`.
myProjectParameterized :: RawScriptExport
myProjectParameterized =
  RawScriptExport
    "1.2.3"
    ( fromList
        [ ("alwaysSucceeds", either (error . unpack) id $ mkEnvelope def "alwaysSucceedsParam" alwaysSucceedsParam)
        ]
    )

-- This is example script linker.
myProjectLinker :: Linker Integer (ScriptExport ())
myProjectLinker = do
  as <- fetchTS @( 'ValidatorRole) @'[Integer] "alwaysSucceeds"
  arg <- exportParam

  return $
    ScriptExport
      "1.2.3"
      ( fromList
          [ ("alwaysSucceeds", toScript $ as Ply.# arg)
          ]
      )
      ()
