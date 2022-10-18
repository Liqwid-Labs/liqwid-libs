{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
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
  insertStaticBuilder,
  insertScriptExportWithLinker,
  toList,
) where

import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Default.Class (Default (def))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import GHC.Generics qualified as GHC
import Optics.TH (makeFieldLabelsNoPrefix)
import ScriptExport.ScriptInfo (
  Linker,
  RawScriptExport,
  ScriptExport,
  runLinker,
 )
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
          { Servant.errBody = (LBS.pack . unpack) err
          }
    toServantErr (Right x) = pure x

{- | Possible data to request.

     @since 2.0.0
-}
data ServeElement where
  ServeRawScriptExport ::
    forall (a :: Type) (param :: Type).
    (Aeson.FromJSON param, Aeson.ToJSON a) =>
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

{- | Handle `ServeElement` and returns JSON.

     @since 2.0.0
-}
handleServe :: Maybe Aeson.Value -> ServeElement -> Except Text Aeson.Value
handleServe _ (ServeJSON x) = pure $ Aeson.toJSON x
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
handleServe Nothing (ServeJSONWithParam _) = throwError "Query expects an argument, but nothing is given"

{- | Represents a list of named pure functions.

     @since 2.0.0
-}
newtype Builders
  = Builders (Map Text ServeElement)
  deriving
    ( -- | @since 1.0.0
      Semigroup
    , -- | @since 1.0.0
      Monoid
    )
    via (Map Text ServeElement)

-- | @since 2.0.0
getBuilders ::
  Builders ->
  Map Text ServeElement
getBuilders (Builders b) = b

{- | Get a list of the available builders.

 @since 2.0.0
-}
toList :: Builders -> [Text]
toList = Map.keys . getBuilders

-- | @since 2.0.0
instance Default Builders where
  def = Builders Map.empty

{- | Insert a pure function into the Builders map.

     @since 2.0.0
-}
insertBuilder ::
  forall (p :: Type) (s :: Type).
  (Aeson.FromJSON p, Aeson.ToJSON s) =>
  Text ->
  (p -> s) ->
  Builders
insertBuilder k f =
  Builders $ Map.insert k (ServeJSONWithParam f) mempty

insertStaticBuilder ::
  forall (a :: Type).
  (Aeson.ToJSON a) =>
  Text ->
  a ->
  Builders
insertStaticBuilder k x =
  Builders $ Map.insert k (ServeJSON x) mempty

{- | Insert a 'RawScriptExport' and 'ScriptLinker' to the Builders Map. The
     builder will return applied `ScriptExport` with given parameter.

     @since 2.0.0
-}
insertScriptExportWithLinker ::
  forall (param :: Type) (a :: Type).
  (Aeson.FromJSON param, Aeson.ToJSON a) =>
  Text ->
  RawScriptExport ->
  Linker param (ScriptExport a) ->
  Builders
insertScriptExportWithLinker k scr linker =
  Builders $ Map.insert k (ServeRawScriptExport scr linker) mempty

----------------------------------------
-- Field Labels

-- | @since 2.0.0
makeFieldLabelsNoPrefix ''ScriptQuery
