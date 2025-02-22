module Text.Dot.Build
  ( node
  , edge
  , (-->)
  , subgraphWith
  , subgraph
  , subgraphWith_
  , subgraph_
  , clusterWith
  , cluster
  , clusterWith_
  , cluster_
  ) where

import "this" Prelude

import Control.Lens
import Data.List.NonEmpty  qualified as NE

import Text.Dot.Attributes
import Text.Dot.Monad
import Text.Dot.Types


--------------------------------------------------------------------------------
-- Entity creation functions

-- | Creates a node in the graph, at the current t'Path', with the given label.
--
-- The newly created node will be assigned all of the default 'Node' attributes
-- (see 'defaults'). This returns a new t'Entity' that uniquely identifies this
-- node in the graph, with the attribute "label" set to the given argument.
--
-- This function updates the 'its' entity to this node.
node :: MonadDot m => Text -> m Entity
node desc = do
  entity <- register Node
  its label ?= desc
  pure entity

-- | Creates an edge in the graph, at the current t'Path'.
--
-- The newly created edge will be assigned all of the default 'Edge' attributes
-- (see 'defaults'). This returns a new t'Entity' that uniquely identifies this
-- edge in the graph.
--
-- If an entity is a cluster, we set the graph's "compound" property to true,
-- and we attempt to locate any node within it. If there isn't any, we fail
-- silently by outputing a valid but unexpected edge.
--
-- This function updates the 'its' entity to this edge.
edge :: MonadDot m => Entity -> Entity -> m Entity
edge a b = do
  na <- getTail a
  nb <- getHead b
  entity <- register Edge
  edgeInfo . at entity ?= EdgeInfo a b na nb
  pure entity

-- | Alias for 'edge'.
--
-- This can be used in both directed and undirected graphs: the rendering
-- process will tke care of using the correct symbol in the generated DOT file.
--
-- > graph do
-- >   x <- node "x"
-- >   y <- node "y"
-- >   z <- node "z"
-- >   x --> y
-- >   x --> z
--
-- This function updates the 'its' entity to this edge.
(-->) :: MonadDot m => Entity -> Entity -> m Entity
(-->) = edge

-- | Creates a subgraph in the given context.
--
-- The newly created subgraph will be assigned all of the default 'Subgraph'
-- attributes (see 'defaults'). The argument to this function is a callback that
-- takes the newly minted t'Entity' and creates the corresponding subgraph.
--
-- This function updates the 'its' entity to this node *twice*: before executing
-- the callback, and before returning.
--
-- > graph do
-- >   (subgraphID, nodeID) <- subgraphWith \subgraphID -> do
-- >     its fontcolor ?= "green" -- points to the subgraph
-- >     x <- node "x"
-- >     its fontcolor ?= "red"   -- points to node "x"
-- >     pure x
-- >   use (its fontcolor)        -- points to the subgraph, returns green
--
-- This returns a pair containing the subgraph's t'Entity' and the result of the
-- subexpression.
subgraphWith :: MonadDot m => (Entity -> m a) -> m (Entity, a)
subgraphWith = recurse Subgraph

-- | Like 'subgraphWith', but the subexpression doesn't take the t'Entity' as
-- argument.
subgraph :: MonadDot m => m a -> m (Entity, a)
subgraph = recurse Subgraph . const

-- | Like 'subgraphWith', but does not return the subgraph's t'Entity'.
subgraphWith_ :: MonadDot m => (Entity -> m a) -> m a
subgraphWith_ = fmap snd . recurse Subgraph

-- | Like 'subgraphWith', but the subexpression doesn't take the t'Entity' as
-- argument, and it does not return the subgraph's t'Entity'.
subgraph_ :: MonadDot m => m a -> m a
subgraph_ = fmap snd . recurse Subgraph . const

-- | Like 'subgraphWith', but creates a cluster instead.
--
-- The created entity will use the default 'Cluster' attributes.
clusterWith :: MonadDot m => (Entity -> m a) -> m (Entity, a)
clusterWith = recurse Cluster

-- | Like 'clusterWith', but the subexpression doesn't take the t'Entity' as
-- argument.
cluster :: MonadDot m => m a -> m (Entity, a)
cluster = recurse Cluster . const

-- | Like 'clusterWith', but does not return the cluster's t'Entity'.
clusterWith_ :: MonadDot m => (Entity -> m a) -> m a
clusterWith_ = fmap snd . recurse Cluster

-- | Like 'clusterWith', but the subexpression doesn't take the t'Entity' as
-- argument, and it does not return the cluster's t'Entity'.
cluster_ :: MonadDot m => m a -> m a
cluster_ = fmap snd . recurse Cluster . const


--------------------------------------------------------------------------------
-- Internal helpers

recurse :: MonadDot m => EntityType -> (Entity -> m a) -> m (Entity, a)
recurse etype callback = do
  entity <- register etype
  contextStack %= NE.cons mempty
  result <- withPath entity $ callback entity
  sub <- popContext
  subgraphInfo . at entity ?= sub
  latest .= entity
  pure (entity, result)

register :: MonadDot m => EntityType -> m Entity
register etype = do
  suffix <- use entityIndex
  let entity = Entity etype suffix
  defAttrs <- use $ defaultAttributes . at etype . non mempty
  context <>:= [entity]
  entityIndex += 1
  attributes entity .= defAttrs
  latest .= entity
  pure entity

getTail, getHead :: MonadDot m => Entity -> m Entity
getTail eid =
  case getType eid of
    Cluster -> do
      g <- rootGraph
      attributes g . compound ?= "true"
      fromMaybe eid <$> locateNode eid
    _ -> pure eid
getHead eid =
  case getType eid of
    Cluster -> do
      g <- rootGraph
      attributes g . compound ?= "true"
      fromMaybe eid <$> locateNode eid
    _ -> pure eid

locateNode :: MonadDot m => Entity -> m (Maybe Entity)
locateNode e = do
  dg <- get
  pure $ e ^? go dg
  where
    go dg f eid =
      case getType eid of
        Cluster  -> foldMapOf (subgraphInfo . at eid . traverse . traverse) (go dg f) dg
        Subgraph -> foldMapOf (subgraphInfo . at eid . traverse . traverse) (go dg f) dg
        Node     -> f eid
        Edge     -> mempty
