{- | Module     : ScriptExport.API
     Maintainer : seungheon@mlabs.city
     Description: Stdout script exporter

     Stdout script exporter.
-}
module ScriptExport.Stdout (
  runStdout,
) where

import ScriptExport.Options (StdoutOptions)
import ScriptExport.Types (Builders, getBuilders, handleServe)

import Control.Monad.Except (runExcept)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString qualified as BS
import Data.Map ((!?))
import Data.Text (unpack)
import Optics (view)
import System.Exit (die)

-- | @since 2.3.0
runStdout :: Builders -> StdoutOptions -> IO ()
runStdout builders options = do
  param <-
    Aeson.decodeStrict'
      <$> maybe BS.getContents BS.readFile (view #param options)

  let builder = view #builder options

  case getBuilders builders !? builder of
    Just b ->
      case runExcept $ handleServe param b of
        Left err -> die . unpack $ "Script compilation failed:\n\t" <> err
        Right x -> BS.putStr . BS.toStrict . encodePretty $ x
    Nothing -> die . unpack $ builder <> " does not exist"
