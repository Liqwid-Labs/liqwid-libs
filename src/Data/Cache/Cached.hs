{- | Module     : API
   Maintainer : emi@haskell.fyi
   Description: API for script exporter.

   API for script exporter.
-}
module Data.Cache.Cached (
  cached,
  cachedM,
  cachedForM,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Cache qualified as Cache
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import System.Clock (TimeSpec)

{- | 'cachedFor' but items last forever.
     Uses a HashMap under the hood.
-}
cached ::
  forall
    (f :: Type -> Type)
    (g :: Type -> Type)
    (k :: Type)
    (v :: Type).
  (Monad f, MonadIO f, Monad g, MonadIO g, Hashable k, Ord k) =>
  (k -> v) ->
  f (k -> g v)
cached f = cachedForM Nothing (pure . f)

{- | 'cachedFor' but items last forever.
     Uses a HashMap under the hood.
-}
cachedM ::
  forall
    (f :: Type -> Type)
    (g :: Type -> Type)
    (k :: Type)
    (v :: Type).
  (Monad f, MonadIO f, Monad g, MonadIO g, Hashable k, Ord k) =>
  (k -> g v) ->
  f (k -> g v)
cachedM = cachedForM Nothing

{- | Create a cached version of a function tainting result with MonadIO context.
     If a 'TimeSpec' is passed, then items will be cached for that long.
     Uses a HashMap under the hood.
-}
cachedForM ::
  forall
    (f :: Type -> Type)
    (g :: Type -> Type)
    (k :: Type)
    (v :: Type).
  (Monad f, MonadIO f, Monad g, MonadIO g, Hashable k, Ord k) =>
  Maybe TimeSpec ->
  (k -> g v) ->
  f (k -> g v)
cachedForM t f =
  liftIO (Cache.newCache t) <&> \cache k -> do
    res <- liftIO $ Cache.lookup cache k
    case res of
      Nothing -> do
        v <- f k
        liftIO $ Cache.insert cache k v
        pure v
      Just v -> do
        pure v
