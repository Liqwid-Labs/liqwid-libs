module ScriptExport.File (
  runFile,
) where

import ScriptExport.Options (FileOptions)
import ScriptExport.Types (Builders, getBuilders, handleServe)

import Control.Monad.Except (runExcept)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Data.Map (toList)
import Data.Text (Text)
import Optics (view)
import Control.Monad (forM)
import Control.Applicative (optional)

runFile :: Text -> Builders -> FileOptions -> IO ()
runFile _revision builders options = do
  paramFile <- optional $ BS.readFile (view #param options)
  param <- pure $ paramFile >>= Aeson.decodeStrict'

  forM (toList $ getBuilders builders) $ \(n, s) -> do
    case runExcept $ handleServe param s of
      Left err -> putStrLn . unpack $ "Failed " <> n <> ": " <> err
      Right x -> do
        LBS.writeFile (view #out options <> "/" <> unpack n <> ".json") . encodePretty $ x
        putStrLn . unpack $  "Wrote " <> n

  putStrLn "Success!"
