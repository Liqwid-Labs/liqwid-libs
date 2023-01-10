module ScriptExport.File (
  runFile,
) where

import ScriptExport.Options (FileOptions)
import ScriptExport.Types (Builders, getBuilders, handleServe)

import Control.Applicative (optional)
import Control.Monad.Except (runExcept)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map ((!?))
import Data.Text (unpack)
import Optics (view)
import System.Exit (die)

{- | Exports 'Builders' into file.

 @since 2.2.0
-}
runFile :: Builders -> FileOptions -> IO ()
runFile builders options = do
  paramFile <- optional $ BS.readFile (view #param options)
  let param = paramFile >>= Aeson.decodeStrict'
      builder = view #builder options

  case getBuilders builders !? builder of
    Just b ->
      case runExcept $ handleServe param b of
        Left err -> die . unpack $ "Script compilation failed:\n\t" <> err
        Right x -> do
          LBS.writeFile
            (view #out options <> "/" <> unpack builder <> ".json")
            (encodePretty x)
          putStrLn . unpack $ "Wrote " <> builder
    Nothing -> die . unpack $ builder <> " does not exist"
