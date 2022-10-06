{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module     : ScriptExport.Options
     Maintainer : emi@haskell.fyi
     Description: Command line options for 'plutarch-script-export'.

     Command line options for 'plutarch-script-export'.
-}
module ScriptExport.Options (
  Options (..),
  ServerOptions (..),
  FileOptions (..),
  ExporterInfo (..),
  parseOptions,
) where

import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Network.Wai.Handler.Warp qualified as Warp
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Applicative ((<**>), (<|>))
import Options.Applicative qualified as Opt

{- | Information about the exporter.

     @since 2.0.0
-}
data ExporterInfo = ExporterInfo
  { revision :: Text
  , exposedBuilders :: [Text]
  }
  deriving anyclass
    ( -- | @since 1.0.0
      Aeson.ToJSON
    , -- | @since 1.0.0
      Aeson.FromJSON
    )
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      GHC.Generic
    )

{- | Command line options for the export server.

     @since 2.0.0
-}
data Options
  = ServerOption ServerOptions
  | FileOption FileOptions
  deriving stock (Show, Eq)

-- | @since 2.0.0
data ServerOptions = ServerOptions
  { port :: Warp.Port
  -- ^ Which port to listen on.
  , enableCorsMiddleware :: Bool
  -- ^ Should we enable CORS middleware for debugging purposes?
  , cacheLifetime :: Int64
  -- ^ How long to keep cached items alive for.
  }
  deriving stock (Show, Eq)

-- | @since 2.0.0
data FileOptions = FileOptions
  { out :: FilePath
  -- ^ Where to write files to.
  , param :: Maybe FilePath
  -- ^ Script parameter.
  }
  deriving stock (Show, Eq)

fileOpt :: Opt.Parser FileOptions
fileOpt =
  FileOptions
    <$> Opt.strOption
      ( Opt.long "out"
          <> Opt.short 'o'
          <> Opt.metavar "OUT"
          <> Opt.value "."
          <> Opt.help "Where to write files to."
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "param"
          <> Opt.short 'p'
          <> Opt.metavar "PARAM"
          <> Opt.value Nothing
          <> Opt.help "Parameters to apply"
      )

serverOpt :: Opt.Parser ServerOptions
serverOpt =
  ServerOptions
    <$> Opt.option
      Opt.auto
      ( Opt.long "port"
          <> Opt.short 'p'
          <> Opt.metavar "PORT"
          <> Opt.value 3939
          <> Opt.help "The port to run the server on."
      )
    <*> Opt.switch
      ( Opt.long "enable-cors-middleware"
          <> Opt.short 'c'
          <> Opt.help
            ( unwords
                [ "Enable CORS middleware."
                , "This is usually required for some local servers."
                , "For security reasons, this should be disabled in production."
                ]
            )
      )
    <*> Opt.option
      Opt.auto
      ( Opt.long "cache-lifetime"
          <> Opt.metavar "CACHE_LIFETIME"
          <> Opt.value 3600
          <> Opt.help "How many seconds should the server keep cached results available for."
      )

opt :: Opt.Parser Options
opt =
  FileOption <$> fileOpt
    <|> ServerOption <$> serverOpt

{- | Parse 'Options' from the command line arguments.

     @since 1.0.0
-}
parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (opt <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Script exporting server for plutus/plutarch scripts."
        )

----------------------------------------
-- Field Labels

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''Options

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''ServerOptions

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''FileOptions
