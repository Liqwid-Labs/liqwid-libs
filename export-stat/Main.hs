import Optics (view)
import System.Exit (die)
import Data.Aeson qualified as Aeson
import System.Environment (getArgs, getProgName)
import System.FilePath (takeBaseName)
import GHC.Generics qualified as GHC
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import ScriptExport.ScriptInfo (RawScriptExport, ScriptExport)
import Data.Map (Map, fromList, toList)
import Data.Text (Text)
import Control.Applicative ((<|>))
import Data.Aeson.Encode.Pretty (encodePretty)
import Codec.Serialise (serialise)
import Plutarch.Evaluate (evalScript)
import PlutusLedgerApi.V2 (
  ExBudget (ExBudget),
  ExCPU (..),
  ExMemory (..),
  Script,
 )
import Ply (TypedScriptEnvelope(tsScript))

data FileFormats
  = Raw RawScriptExport
  | Export (ScriptExport ())
  | MultipleExport (Map Text (ScriptExport ()))
  deriving stock Show

data ScriptStats
  = ScriptStats
  { size :: Int
  , cpuBudget :: ExCPU
  , memBudget :: ExMemory
  }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (Aeson.ToJSON)

help :: IO a
help = do
  pname <- getProgName
  die $ "Usage: " <> pname <> " [export/raw export file]"

decodeExport :: FilePath -> IO (Either String FileFormats)
decodeExport filename = do
  content <- BS.readFile filename <|> help

  return $
    Raw <$> Aeson.eitherDecodeStrict content
    <|> Export <$> Aeson.eitherDecodeStrict content
    <|> MultipleExport <$> Aeson.eitherDecodeStrict content

makeStats :: FileFormats -> Map Text ScriptStats
makeStats =
  \case
    Raw e -> (go . tsScript) <$> view #rawScripts e
    Export e -> go <$> view #scripts e
    MultipleExport e ->
      fromList $
        foldMap (\(k, x) -> (\(k', x') -> (k <> "-" <> k', go x')) <$>
                  toList (view #scripts x)) $ toList e
  where
    go :: Script -> ScriptStats
    go script =
      let (_res, ExBudget cpu mem, _traces) = evalScript script
          size = SBS.length . SBS.toShort . LBS.toStrict . serialise $ script
      in ScriptStats size cpu mem

main :: IO ()
main = do
  exportFile <-
    getArgs >>= \case
      (x: _) -> return x
      _ -> help

  exportContent' <- decodeExport exportFile

  case exportContent' of
    Left e -> die $ "Failed to parse: " <> e
    Right exportContent ->
      LBS.writeFile (takeBaseName exportFile <> "-stats.json") . encodePretty $
        makeStats exportContent
