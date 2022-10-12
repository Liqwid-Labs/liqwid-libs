{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : ScriptExport.Types
Maintainer : emi@haskell.fyi
Description: Param and script types for generation.

Param and script types for generation.
-}
module ScriptExport.Types (
  ServeElement (..),
  ScriptQuery (..),
  Builders,
  handleServe,
  getBuilders,
  runQuery,
  insertBuilder,
  insertScriptExport,
  insertStaticBuilder,
  insertScriptExportWithLinker,
  toList,
) where

import ScriptExport.ScriptInfo
import Data.Kind

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
  , param :: Maybe Aeson.Value
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
    (toServantErr . runExcept . handleServe param)
    . Map.lookup name
    . getBuilders
  where
    toServantErr (Left err) =
      Servant.throwError
        Servant.err400
        {Servant.errBody = (LBS.pack . unpack) err}
    toServantErr (Right x) = pure x

data ServeElement where
  ServeScriptExport ::
    forall (a :: Type).
    (Aeson.ToJSON a) =>
    ScriptExport a ->
    ServeElement

  ServeRawScriptExport ::
    forall (a :: Type) (param :: Type).
    (Aeson.FromJSON param , Aeson.ToJSON a) =>
    RawScriptExport ->
    Linker param (ScriptExport a) ->
    ServeElement

  ServeJSON ::
    forall (s :: Type).
    (Aeson.ToJSON s) =>
    s ->
    ServeElement

  ServeJSONWithParam ::
    forall (p :: Type) (s :: Type).
    (Aeson.FromJSON p, Aeson.ToJSON s) =>
    (p -> s) ->
    ServeElement

handleServe :: Maybe Aeson.Value -> ServeElement -> Except Text Aeson.Value
handleServe _ (ServeJSON x) = pure $ Aeson.toJSON x
handleServe _ (ServeScriptExport x) = pure $ Aeson.toJSON x
handleServe (Just arg) (ServeJSONWithParam f) =
  case Aeson.fromJSON arg of
    Aeson.Error e ->
      throwError $ pack e
    Aeson.Success v' ->
      pure . Aeson.toJSON $ f v'
handleServe (Just arg) (ServeRawScriptExport scr linker) =
  case Aeson.fromJSON arg of
    Aeson.Error e ->
      throwError $ pack e
    Aeson.Success v' ->
      case runLinker linker scr v' of
        Left e -> throwError . pack . show $ e
        Right x -> pure . Aeson.toJSON $ x
handleServe Nothing (ServeRawScriptExport scr _) = pure $ Aeson.toJSON scr
handleServe _ _ = throwError "Incorrect Parameter"

{- | Represents a list of named pure functions.

     @since 1.0.0
-}
newtype Builders
  = Builders (Map Text ServeElement)

getBuilders ::
  Builders ->
  Map Text ServeElement
getBuilders (Builders b) = b

-- | @since 1.0.0
instance Default Builders where
  def = Builders Map.empty

{- | Insert a pure function into the Builders map.

     @since 1.0.0
-}
insertBuilder ::
  forall (p :: Type) (s :: Type).
  (Aeson.FromJSON p, Aeson.ToJSON s) =>
  Text ->
  (p -> s) ->
  Builders ->
  Builders
insertBuilder k f =
  coerce $ Map.insert k (ServeJSONWithParam f)

insertStaticBuilder ::
  forall (a :: Type).
  (Aeson.ToJSON a) =>
  Text ->
  a ->
  Builders ->
  Builders
insertStaticBuilder k x =
  coerce $ Map.insert k (ServeJSON x)

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
  coerce $ Map.insert k (ServeScriptExport scr)

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
  coerce $ Map.insert k (ServeRawScriptExport scr linker)

----------------------------------------
-- Field Labels

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''ScriptQuery
