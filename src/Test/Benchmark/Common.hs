{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Benchmark.Common (
  ToTitle (..),
  ImplData (..),
  MultiImplData (..),
  MultiImplComparisonData (..),
  multiImplData,
) where

import Data.Csv (ToField (toField))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Optics.TH (makeFieldLabelsNoPrefix)

class ToTitle a where
  toTitle :: a -> Text

{- | A value tagged with the name of the implementation that it is associated
 with.
-}
data ImplData a = ImplData
  { name :: Text
  -- ^ the name of the implementation
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''ImplData

instance ToTitle (ImplData a) where
  toTitle = (.name)

instance
  ToField v =>
  ToField (ImplData v)
  where
  toField implData =
    encodeUtf8 (toTitle implData) <> " (" <> toField implData.val <> ")"

-- | A value associated with multiple implementations.
data MultiImplData a = MultiImplData
  { name :: Text
  -- ^ the name of the group of implementations
  , implNames :: [Text]
  -- ^ the names of the individual implementations
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''MultiImplData

multiImplData :: Text -> [ImplData a] -> MultiImplData [ImplData a]
multiImplData name list =
  MultiImplData {name, implNames = map (.name) list, val = list}

instance ToTitle (MultiImplData a) where
  toTitle a = a.name <> " implementations"

-- | A value associated with multiple implementations.
data MultiImplComparisonData a = MultiImplComparisonData
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

instance ToTitle (MultiImplComparisonData a) where
  toTitle a = a.name <> " implementations " <> a.comparisonName
