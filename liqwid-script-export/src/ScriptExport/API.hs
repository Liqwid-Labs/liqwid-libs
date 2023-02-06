{- | Module     : ScriptExport.API
     Maintainer : emi@haskell.fyi
     Description: API for script exporter.

     API for script exporter.
-}
module ScriptExport.API (
  API,
  runServer,
) where

import Codec.Serialise.Orphans ()
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as Aeson
import Data.Cache.Cached (cachedForM)
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Optics (view)
import Prettyprinter (
  Pretty (pretty),
  defaultLayoutOptions,
  hsep,
  layoutPretty,
  viaShow,
 )
import Prettyprinter.Render.String (renderString)
import ScriptExport.Options (ExporterInfo (..), ServerOptions (..))
import ScriptExport.Types (Builders, ScriptQuery (ScriptQuery), runQuery)
import ScriptExport.Types qualified as Builders
import Servant.API (Capture, Get, JSON, Post, ReqBody, (:<|>) (..), type (:>))
import Servant.Server qualified as Servant
import System.Clock (TimeSpec (TimeSpec))
import System.Posix.Signals (Handler (Catch), installHandler, sigKILL, sigTERM)
import Text.Printf (printf)

{- | Servant API type for script generation.

     @since 2.0.0
-}
type API =
  -- POST /query-script/:name
  "query-script"
    :> Capture "name" Text
    :> ReqBody '[JSON] Aeson.Value
    :> Post '[JSON] Aeson.Value
    -- GET /query-script/:name
    :<|> "query-script"
      :> Capture "name" Text
      :> Get '[JSON] Aeson.Value
    -- GET /info
    :<|> "info"
      :> Get '[JSON] ExporterInfo

{- | Run a Warp server that exposes a script generation endpoint.

     @since 2.2.0
-}
runServer :: MonadIO m => Builders -> ServerOptions -> m ()
runServer builders options = do
  let logger req status _maybeFileSize =
        putStrLn . renderString . layoutPretty defaultLayoutOptions $
          hsep
            [ "[info]"
            , viaShow $ Wai.requestMethod req
            , viaShow $ Wai.rawPathInfo req
            , "(" <> pretty (Http.statusCode status) <> ")"
            ]

      settings =
        Warp.defaultSettings
          & Warp.setPort (view #port options)
          & Warp.setLogger logger
          & Warp.setTimeout (view #timeout options)
          & Warp.setInstallShutdownHandler shutdownHandler

      corsPolicy =
        simpleCorsResourcePolicy
          { -- NOTE: Webpack dev server requires this for CORS workaround.
            corsRequestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
          }

      corsMiddleware = cors . const $ Just corsPolicy

      serverInfo =
        ExporterInfo $ Builders.toList builders

  -- Scripts stay cached for the amount of time specified by the `cacheLifetime` option.
  query <- cachedForM (Just $ TimeSpec (view #cacheLifetime options) 0) (`runQuery` builders)

  let handler =
        (\name param -> query $ ScriptQuery name (Just param))
          :<|> (\name -> query $ ScriptQuery name Nothing)
          :<|> pure serverInfo

  liftIO $ printf "[info] Running script export server on :%d\n" (Warp.getPort settings)

  Servant.serve (Proxy @API) handler
    & (if view #enableCorsMiddleware options then corsMiddleware else id)
    & Warp.runSettings settings
    & liftIO
  where
    shutdownHandler :: IO () -> IO ()
    shutdownHandler closeSocket = do
      void $ installHandler sigTERM (Catch closeSocket) Nothing
      void $ installHandler sigKILL (Catch closeSocket) Nothing
