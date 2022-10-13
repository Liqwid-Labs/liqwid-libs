module ScriptExport.File (
  runFile,
) where

import ScriptExport.Options (FileOptions)
import ScriptExport.Types (Builders, getBuilders, handleServe)

import Control.Applicative (optional)
import Control.Monad (forM_)
import Control.Monad.Except (runExcept)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map (toList)
import Data.Text (Text, unpack)
import Optics (view)

{- | Exports 'Builders' into file.

 @since 2.0.0
-}
runFile :: Text -> Builders -> FileOptions -> IO ()
runFile _revision builders options = do
  paramFile <- optional $ BS.readFile (view #param options)
  let param = paramFile >>= Aeson.decodeStrict'

  forM_ (toList $ getBuilders builders) $ \(n, s) -> do
    case runExcept $ handleServe param s of
      Left err -> putStrLn . unpack $ "Skipped " <> n <> ": " <> err
      Right x -> do
        LBS.writeFile (view #out options <> "/" <> unpack n <> ".json") $
          encodePretty x
        putStrLn . unpack $ "Wrote " <> n
