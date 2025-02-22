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
-- default attributes (see 'Text.Dot.defaults').
data EntityType = Node | Edge | Subgraph | Cluster
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Hashable EntityType where
  hashWithSalt s e = hashWithSalt s (fromEnum e)

-- | Opaque identifier for graph entities.
--
-- This type uniquely identifies an entity within the graph. To create one, see
-- 'Text.Dot.node', 'Text.Dot.edge', 'Text.Dot.subgraph', or 'Text.Dot.cluster'.
data Entity = Entity EntityType Int
  deriving (Eq, Ord)

instance Hashable Entity where
  hashWithSalt s (Entity t i) = s `hashWithSalt` t `hashWithSalt` i

-- | Retrieves the type of a given t'Entity'.
getType :: Entity -> EntityType
getType (Entity t _) = t


--------------------------------------------------------------------------------
-- Internal state

-- | An entity's attributes.
--
-- Attributes are untyped, and are a simple mapping from 'Text' to 'Text', for
-- flexibility.
type Attributes = HashMap Text Text

-- | A path through the graph.
--
-- This opaque type represents the path from the root to the current scope. The
-- current path can be obtained via 'Text.Dot.path'.
newtype Path = Path { unwrapPath :: NonEmpty Entity }

makePrisms ''Path

type DotContext = [Entity]

data EdgeInfo = EdgeInfo Entity Entity Entity Entity

-- | Internal opaque graph state.
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
