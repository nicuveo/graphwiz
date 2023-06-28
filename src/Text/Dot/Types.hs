{-# LANGUAGE TemplateHaskell #-}

module Text.Dot.Types where

import "this" Prelude

import Control.Lens
import Data.Hashable


--------------------------------------------------------------------------------
-- Entities

-- | Represents the type of a graph entity.
--
-- DOT distinguishes between graphs, nodes, edges, graphs, and subgraphs /
-- clusters. This type differs slightly: we do not have a type for graphs, as it
-- is never required, and we differentiate subgraphs and clusters.
--
-- This type is used internally to distinguish entities, for the purpose of
-- default attributes (see 'defaults').
data EntityType = Node | Edge | Subgraph | Cluster
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Hashable EntityType where
  hashWithSalt s e = hashWithSalt s (fromEnum e)

-- | Opaque identifier for graph entities.
--
-- This type uniquely identifies an entity within the graph. The only way to
-- create one is by using 'node', 'edge', 'subgraph', or 'cluster'.
data Entity = Entity EntityType Int
  deriving (Eq, Ord)

instance Hashable Entity where
  hashWithSalt s (Entity t i) = s `hashWithSalt` t `hashWithSalt` i

getType :: Entity -> EntityType
getType (Entity t _) = t


--------------------------------------------------------------------------------
-- Internal state

-- | An entity's attributes.
--
-- Attributes are untypes, and are a simple mapping from 'Text' to 'Text, for
-- flexibility.
type Attributes = HashMap Text Text

newtype Path = Path { unwrapPath :: NonEmpty Entity }

makePrisms ''Path

type DotContext = [Entity]

data EdgeInfo = EdgeInfo Entity Entity Entity Entity

data DotGraph = DotGraph
  { _defaultAttributes :: HashMap EntityType Attributes
  , _entityAttributes  :: HashMap Entity Attributes
  , _edgeInfo          :: HashMap Entity EdgeInfo
  , _subgraphInfo      :: HashMap Entity DotContext
  , _contextStack      :: NonEmpty DotContext
  , _entityIndex       :: Int
  , _latest            :: Entity
  }

makeLenses ''DotGraph

initialGraph :: Entity -> DotGraph
initialGraph = DotGraph mempty mempty mempty mempty (pure mempty) 0
