module ScriptExport.File (
  runFile,
) where

import ScriptExport.Options
import ScriptExport.Types

import Control.Monad.Except (runExcept)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map (insert)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Optics (view)

runFile :: Text -> Builders -> FileOptions -> IO ()
runFile revision builders options = do
  param <- Aeson.decodeStrict' <$> BS.readFile (view #param options)

  let applied =
        runExcept
          . traverse ($ fromMaybe (Aeson.toJSON ()) param)
          . getBuilders
          $ builders
  case applied of
    Left err -> putStrLn $ "Builder failed : " <> show err
    Right x ->
      LBS.writeFile (view #out options) . encodePretty $
        insert "rev" (Aeson.toJSON revision) x

  putStrLn "Success!"
