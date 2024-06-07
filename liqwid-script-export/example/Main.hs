{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{- | Module     : Main
     Maintainer : emi@haskell.fyi
     Description: Example usage of 'liqwid-script-export'.

Example usage of 'liqwid-script-export'.
-}
module Main (main) where

import Data.Map (fromList)
import Data.Text (unpack)
import Plutarch (
  Config (Tracing),
  LogLevel (LogInfo),
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.LedgerApi (PScriptContext)
import Plutarch.Prelude (
  ClosedTerm,
  PData,
  PInteger,
  POpaque,
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
import Ply ((#))
import Ply.Plutarch.TypedWriter (mkEnvelope)
import ScriptExport.Export (exportMain)
import ScriptExport.ScriptInfo (
  Linker,
  RawScriptExport (RawScriptExport),
  RoledScript (RoledScript),
  ScriptExport (ScriptExport),
  ScriptRole (ThreeArgumentScript),
  fetchTS,
  getParam,
  mkThreeArgumentScriptInfo,
  toRoledScript,
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
  mconcat
    [ insertStaticBuilder "alwaysSucceeds" (mkThreeArgumentScriptInfo alwaysSucceeds)
    , insertBuilder @Integer
        "alwaysSucceedsParam"
        (\x -> mkThreeArgumentScriptInfo (alwaysSucceedsParam Plutarch.Prelude.# pconstant x))
    , insertStaticBuilder "my-onchain-project" myproject
    , insertScriptExportWithLinker "my-onchain-project-param" myProjectParameterized myProjectLinker
    ]

-- This is our dummy validator.
alwaysSucceeds :: ClosedTerm (PData :--> PData :--> PScriptContext :--> POpaque)
alwaysSucceeds = plam $ \_ _ _ -> popaque (pcon PUnit)

-- This is our dummy paramterized validator.
alwaysSucceedsParam :: ClosedTerm (PInteger :--> PData :--> PData :--> PScriptContext :--> POpaque)
alwaysSucceedsParam = plam $ \x _ _ _ -> unTermCont $ do
  _ <- tcont $ plet $ x + x
  pure $ popaque (pcon PUnit)

-- This is example `ScriptExport`
myproject :: ScriptExport Int
myproject =
  ScriptExport
    ( fromList
        [ ("alwaysSucceeds", RoledScript (either (error . unpack) id $ compile conf alwaysSucceeds) ThreeArgumentScript)
        ]
    )
    10

-- This is example `RawScriptExport`.
myProjectParameterized :: RawScriptExport
myProjectParameterized =
  RawScriptExport $
    fromList
      [ ("alwaysSucceeds", either (error . unpack) id $ mkEnvelope conf "alwaysSucceedsParam" alwaysSucceedsParam)
      ]

conf :: Config
conf = Tracing LogInfo DetTracing

-- This is example script linker.
myProjectLinker :: Linker Integer (ScriptExport ())
myProjectLinker = do
  as <- fetchTS @ThreeArgumentScript @'[Integer] "alwaysSucceeds"
  arg <- getParam

  return $
    ScriptExport
      ( fromList
          [ ("alwaysSucceeds", toRoledScript $ as Ply.# arg)
          ]
      )
      ()
