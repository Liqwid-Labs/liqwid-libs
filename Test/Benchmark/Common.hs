{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Benchmark.Common (ImplData (..), MultiImplData (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.TH (makeFieldLabelsNoPrefix)

-- | A value tagged with the name of the implementation that it is associated with.
data ImplData a = ImplData
  { name :: Text
  -- ^ the name of the implementation
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''ImplData

-- | A value associated with multiple implementations.
data MultiImplData a = MultiImplData
  { name :: Text
  -- ^ the name of the group of implementations
  , names :: [Text]
  -- ^ the names of the individual implementations
  , val :: a
  -- ^ the payload
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)

makeFieldLabelsNoPrefix ''MultiImplData