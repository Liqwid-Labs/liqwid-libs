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
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as Aeson
import Data.Cache.Cached (cachedForM)
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsRequestHeaders), cors, simpleCorsResourcePolicy)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, hsep, layoutPretty, viaShow)
import Prettyprinter.Render.String (renderString)
import ScriptExport.Options (Options (..))
import ScriptExport.Types (Builders, ScriptQuery (ScriptQuery), runQuery)
import ScriptExport.Types qualified as Builders
import Servant.API (Capture, Get, JSON, Post, ReqBody, (:<|>) (..), type (:>))
import Servant.Server qualified as Servant
import System.Clock (TimeSpec (TimeSpec))
import Text.Printf (printf)

{- | Servant API type for script generation.

     @since 1.0.0
-}
type API =
  -- POST /query-script/:name
  "query-script"
    :> Capture "name" Text
    :> ReqBody '[JSON] Aeson.Value
    :> Post '[JSON] Aeson.Value
    -- GET /info
    :<|> "info"
      :> Get '[JSON] ServerInfo

{- | Information about the server.

     @since 1.0.0
-}
data ServerInfo = ServerInfo
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

{- | Run a Warp server that exposes a script generation endpoint.

     @since 1.0.0
-}
runServer :: MonadIO m => Text -> Builders -> Options -> m ()
runServer revision builders options = do
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
          & Warp.setPort options.port
          & Warp.setLogger logger

      corsPolicy =
        simpleCorsResourcePolicy
          { -- NOTE: Webpack dev server requires this for CORS workaround.
            corsRequestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
          }

      corsMiddleware = cors . const $ Just corsPolicy

      serverInfo =
        ServerInfo
          { revision = revision
          , exposedBuilders = Builders.toList builders
          }

  -- Scripts stay cached for five minutes
  query <- cachedForM (Just $ TimeSpec options.cacheLifetime 0) (`runQuery` builders)

  let handler = (\name -> query . ScriptQuery name) :<|> pure serverInfo

  liftIO $ printf "[info] Running script export server on :%d\n" (Warp.getPort settings)

  Servant.serve (Proxy @API) handler
    & (if options.enableCorsMiddleware then corsMiddleware else id)
    & Warp.runSettings settings
    & liftIO
