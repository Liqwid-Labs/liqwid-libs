{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @since 1.0.0
module Plutarch.Benchmark.Common (
  ToTitle (..),
  ImplData (..),
  MultiImplData (..),
  MultiImplComparisonData (..),
  multiImplData,
) where

import Data.Csv (ToField (toField))
import Data.Text (Text)
import Data.Kind (Type)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Optics.TH (makeFieldLabelsNoPrefix)

class ToTitle (a :: Type) where
  toTitle :: a -> Text

{- | A value tagged with the name of the implementation that it is associated
 with.

 @since 1.0.0
-}
data ImplData (a :: Type) = ImplData
  { name :: Text
  -- ^ the name of the implementation
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''ImplData

-- | @since 1.0.0
instance forall (a :: Type). ToTitle (ImplData a) where
  toTitle = (.name)

-- | @since 1.0.0
instance
  forall (v :: Type).
  ToField v =>
  ToField (ImplData v)
  where
  toField implData =
    encodeUtf8 (toTitle implData) <> " (" <> toField implData.val <> ")"

-- | A value associated with multiple implementations.
-- | @since 1.0.0
data MultiImplData (a :: Type) = MultiImplData
  { name :: Text
  -- ^ the name of the group of implementations
  , implNames :: [Text]
  -- ^ the names of the individual implementations
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''MultiImplData

-- | @since 1.0.0
multiImplData :: forall {a :: Type}. Text -> [ImplData a] -> MultiImplData [ImplData a]
multiImplData name list =
  MultiImplData {name, implNames = map (.name) list, val = list}

-- | @since 1.0.0
instance forall (a :: Type). ToTitle (MultiImplData a) where
  toTitle a = a.name <> " implementations"

-- | A value associated with multiple implementations.
-- | @since 1.0.0
data MultiImplComparisonData (a :: Type) = MultiImplComparisonData
  { name :: Text
  -- ^ the name of the group of implementations
  , implNames :: [Text]
  -- ^ the names of the individual implementations
  , comparisonName :: Text
  -- ^ the name of the comparison, i.e. "worst case", "mean"
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''MultiImplComparisonData

-- | @since 1.0.0
instance forall (a :: Type). ToTitle (MultiImplComparisonData a) where
  toTitle a = a.name <> " implementations " <> a.comparisonName
