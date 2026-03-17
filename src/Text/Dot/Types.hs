{-# LANGUAGE TemplateHaskell #-}

module Text.Dot.Types where

import "this" Prelude

import Control.Lens
import Data.Hashable
import TextBuilder    (TextBuilder)


--------------------------------------------------------------------------------
-- Entities

-- | Represents the type of a graph entity.
--
-- DOT distinguishes between graphs, nodes, edges, and subgraphs /
-- clusters. This type differs slightly: it does not have a value for graphs, as it
-- is never required, and we differentiate subgraphs and clusters.
--
-- This type is used internally to distinguish entities, mostly for the purpose
-- of default attributes (see 'Text.Dot.defaults').
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

-- | Unique entity of the top-level graph.
rootGraph :: Entity
rootGraph = Entity Subgraph (-1)

-- | Describes types that can be used as the end of an edge.
--
-- When declaring an edge, each node can be described in two different ways:
-- either via its 'Entity', or by using a 'Text' name.
--
-- For more information, see 'edge', 'fromName', and 'register'.
class ToEdgeNode a where
  toEdgeNode :: a -> EdgeNode

instance ToEdgeNode Entity where
  toEdgeNode = KnownNode

instance ToEdgeNode EdgeNode where
  toEdgeNode = id

-- | Constructs an edge node from the given text name.
--
-- The node will be resolved at a later time; see 'register'.
retrieve :: Text -> EdgeNode
retrieve = UnknownNode


--------------------------------------------------------------------------------
-- Internal state

data EdgeNode
  = KnownNode Entity
  | UnknownNode Text

-- | An entity's attributes.
--
-- Attributes are untyped, and are a simple mapping from 'Text' to 'Text', for
-- flexibility.
type Attributes = HashMap Text Text

-- | A path through the graph.
--
-- This opaque type represents the path from the root to the current scope. The
-- current path can be obtained via 'Text.Dot.currentPath'.
newtype Path = Path { unwrapPath :: NonEmpty Entity }

makePrisms ''Path

type DotContext = [Entity]

data EdgeInfo = EdgeInfo EdgeNode EdgeNode (Maybe Entity) (Maybe Entity)

-- | Internal opaque graph state.
data DotGraph = DotGraph
  { _defaultAttributes :: HashMap EntityType Attributes
  , _entityAttributes  :: HashMap Entity Attributes
  , _edgeInfo          :: HashMap Entity EdgeInfo
  , _subgraphInfo      :: HashMap Entity DotContext
  , _entityRegister    :: HashMap Text Entity
  , _contextStack      :: NonEmpty DotContext
  , _entityIndex       :: Int
  , _latest            :: Entity
  }

makeLenses ''DotGraph

initialGraph :: DotGraph
initialGraph = DotGraph mempty mempty mempty mempty mempty (pure mempty) 0 rootGraph

data RenderState = RenderState
  { _knownNodes   :: HashSet Entity
  , _delayedEdges :: [TextBuilder]
  }

makeLenses ''RenderState

initialRenderState :: RenderState
initialRenderState = RenderState mempty mempty
