{- | Module     : ScriptExport.Options
     Maintainer : emi@haskell.fyi
     Description: Command line options for 'plutarch-script-export'.

     Command line options for 'plutarch-script-export'.
-}
module ScriptExport.Options (Options (..), parseOptions) where

import Data.Int (Int64)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

{- | Command line options for the export server.

     @since 1.0.0
-}
data Options = Options
  { port :: Warp.Port
  -- ^ Which port to listen on.
  , enableCorsMiddleware :: Bool
  -- ^ Should we enable CORS middleware for debugging purposes?
  , cacheLifetime :: Int64
  -- ^ How long to keep cached items alive for.
  }
  deriving stock (Show, Eq)

opt :: Opt.Parser Options
opt =
  Options
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
