{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : ScriptExport.Types
Maintainer : emi@haskell.fyi
Description: Param and script types for generation.

Param and script types for generation.
-}
module ScriptExport.Types (
  ScriptQuery (..),
  Builders,
  getBuilders,
  runQuery,
  insertBuilder,
  insertScriptExport,
  insertScriptExportWithLinker,
  toList,
) where

import ScriptExport.ScriptInfo

import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Coerce (coerce)
import Data.Default.Class (Default (def))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import GHC.Generics qualified as GHC
import Optics.TH (makeFieldLabelsNoPrefix)
import Servant qualified

{- | Query data for getting script info.

     @since 1.0.0
-}
data ScriptQuery = ScriptQuery
  { name :: Text
  , paramsPayload :: Aeson.Value
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
    , -- | @since 1.0.0
      Ord
    )
  deriving anyclass
    ( -- | @since 1.0.0
      Hashable
    )

{- | Run a query on Builders.

     @since 1.0.0
-}
runQuery :: ScriptQuery -> Builders -> Servant.Handler Aeson.Value
runQuery (ScriptQuery name param) =
  maybe
    (Servant.throwError Servant.err404 {Servant.errBody = "Builder not found"})
    (toServantErr . runExcept . ($ param))
    . Map.lookup name
    . getBuilders
  where
    toServantErr (Left err) = Servant.throwError Servant.err400 {Servant.errBody = (LBS.pack . unpack) err}
    toServantErr (Right x) = pure x

{- | Represents a list of named pure functions.

     @since 1.0.0
-}
newtype Builders
  = Builders (Map Text (Aeson.Value -> Except Text Aeson.Value))

getBuilders ::
  Builders ->
  Map Text (Aeson.Value -> Except Text Aeson.Value)
getBuilders (Builders b) = b

-- | @since 1.0.0
instance Default Builders where
  def = Builders Map.empty

{- | Insert a pure function into the Builders map.

     @since 1.0.0
-}
insertBuilder ::
  forall p s.
  (Aeson.FromJSON p, Aeson.ToJSON s) =>
  Text ->
  (p -> s) ->
  Builders ->
  Builders
insertBuilder k = coerce . Map.insert k . throughJSON
  where
    throughJSON ::
      forall p s.
      (Aeson.FromJSON p, Aeson.ToJSON s) =>
      (p -> s) ->
      (Aeson.Value -> Except Text Aeson.Value)
    throughJSON f v =
      case Aeson.fromJSON v of
        Aeson.Error e ->
          throwError $ pack e
        Aeson.Success v' -> pure . Aeson.toJSON $ f v'

{- | Get a list of the available builders.

     @since 1.0.0
-}
toList :: Builders -> [Text]
toList = Map.keys . getBuilders

{- | Insert a 'ScriptExport' to the Builders Map. It will not require a
     parameter since 'ScriptExport' is ready-to-go script.

     @since 2.0.0
-}
insertScriptExport ::
  forall a.
  (Aeson.ToJSON a) =>
  Text ->
  ScriptExport a ->
  Builders ->
  Builders
insertScriptExport k scr =
  insertBuilder @() k (const scr)

{- | Insert a 'RawScriptExport' and 'ScriptLinker' to the Builders Map. The
     builder will return applied `ScriptExport` with given parameter.

     @since 2.0.0
-}
insertScriptExportWithLinker ::
  forall param a.
  (Aeson.FromJSON param, Aeson.ToJSON a) =>
  Text ->
  RawScriptExport ->
  Linker param (ScriptExport a) ->
  Builders ->
  Builders
insertScriptExportWithLinker k scr linker =
  insertBuilder k (runLinker linker scr)

----------------------------------------
-- Field Labels

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''ScriptQuery
