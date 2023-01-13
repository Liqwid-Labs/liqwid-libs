{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module     : ScriptExport.Options
     Maintainer : emi@haskell.fyi
     Description: Command line options for 'liqwid-script-export'.

     Command line options for 'liqwid-script-export'.
-}
module ScriptExport.Options (
  Options (..),
  ServerOptions (..),
  FileOptions (..),
  StdoutOptions (..),
  ExporterInfo (..),
  parseOptions,
) where

import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Network.Wai.Handler.Warp qualified as Warp
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

{- | Information about the exporter.

     @since 2.2.0
-}
newtype ExporterInfo = ExporterInfo
  { exposedBuilders :: [Text]
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

     @since 2.2.0
-}
data Options
  = ServerOption ServerOptions
  | FileOption FileOptions
  | StdoutOption StdoutOptions
  | ListBuilders
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

-- | @since 2.3.0
data ServerOptions = ServerOptions
  { port :: Warp.Port
  -- ^ Which port to listen on.
  , enableCorsMiddleware :: Bool
  -- ^ Should we enable CORS middleware for debugging purposes?
  , cacheLifetime :: Int64
  -- ^ How long to keep cached items alive for.
  , timeout :: Int
  -- ^ Timeout for Warp web server, in seconds.
  }
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

-- | @since 2.0.0
data FileOptions = FileOptions
  { out :: FilePath
  -- ^ Where to write files to.
  , param :: FilePath
  -- ^ Script parameter.
  , builder :: Text
  -- ^ Builder to use.
  }
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

-- | @since 2.3.0
data StdoutOptions = StdoutOptions
  { param :: Maybe FilePath
  -- ^ Script parameter.
  , builder :: Text
  -- ^ Builder to use.
  }
  deriving stock
    ( -- | @since 2.3.0
      Show
    , -- | @since 2.3.0
      Eq
    )

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
    <*> Opt.strOption
      ( Opt.long "param"
          <> Opt.short 'p'
          <> Opt.metavar "PARAM"
          <> Opt.value ""
          <> Opt.help "Parameters to apply"
      )
    <*> Opt.strOption
      ( Opt.long "builder"
          <> Opt.short 'b'
          <> Opt.metavar "BUILDER"
          <> Opt.help "Builder to use"
      )
    <**> Opt.helper

stdoutOpt :: Opt.Parser StdoutOptions
stdoutOpt =
  StdoutOptions
    <$> Opt.optional
      ( Opt.strOption
          ( Opt.long "param"
              <> Opt.short 'p'
              <> Opt.metavar "PARAM"
              <> Opt.help "Parameters to apply. If none is given, will expect stdin"
          )
      )
    <*> Opt.strOption
      ( Opt.long "builder"
          <> Opt.short 'b'
          <> Opt.metavar "BUILDER"
          <> Opt.help "Builder to use"
      )
    <**> Opt.helper

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
    <*> Opt.option
      Opt.auto
      ( Opt.long "timeout"
          <> Opt.metavar "TIMEOUT"
          <> Opt.value 6000
          <> Opt.help "Script export server timeout in seconds. (default: 10 minutes)"
      )
    <**> Opt.helper

{- | Parse 'Options' from the command line arguments.

     @since 2.2.0
-}
parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    opt =
      Opt.subparser . foldMap mkSubcommand $
        [ (FileOption <$> fileOpt, "file", "Complie scripts using file IO")
        , (ServerOption <$> serverOpt, "serve", "Start script server")
        , (StdoutOption <$> stdoutOpt, "stdout", "Print export to stdout")
        , (pure ListBuilders, "list", "List out all builders")
        ]
    mkSubcommand (p, n, d) =
      Opt.command n $ Opt.info p $ Opt.fullDesc <> Opt.progDesc d
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

-- | @since 2.3.0
makeFieldLabelsNoPrefix ''StdoutOptions
