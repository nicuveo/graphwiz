module Text.Dot.Build
  ( node
  , edge
  , (-->)
  , subgraph
  , subgraph_
  , cluster
  , cluster_
  , registerItAs
  , register
  , retrieve
  ) where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict qualified as M
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
  entity <- record Node
  its label ?= desc
  pure entity

-- | Creates an edge in the graph, at the current t'Path'.
--
-- The newly created edge will be assigned all of the default 'Edge' attributes
-- (see 'defaults'). This returns a new t'Entity' that uniquely identifies this
-- edge in the graph.
--
-- There are two ways to specify each end of an edge: either by directly giving
-- an entity, or by giving an entity's "name", registered via 'register', and
-- obtained via 'retrieve'. The name is only resolved after the entire graph has
-- been processed; this method allows the user to reference a node that has yet
-- to be created, or to reference a node without manually propagating its
-- entity.
--
-- > digraph do
-- >   x <- node "x"
-- >   y <- node "y"
-- >   edge x y
-- >   edge (retrieve "z node") x
-- >   edge y (retrieve "z node")
-- >   z <- node "z"
-- >   registerItAs "z node"
--
-- If any of the two target entiies is a cluster, the root graph's "compound"
-- property will be set to true, and the edge will be adjusted to make use of
-- 'lhead' or 'ltail' accordingly.
--
-- If an edge makes reference to a node that doesn't exist yet (see 'retrieve'),
-- its declaration will be moved to the top level of the graph, but its
-- attributes will remain unchanged.
--
-- This function updates the 'its' entity to this edge.
edge
  :: (ToEdgeNode a, ToEdgeNode b, MonadDot m)
  => a
  -> b
  -> m Entity
edge a b = do
  entity <- record Edge
  edgeInfo . at entity ?= EdgeInfo (toEdgeNode a) (toEdgeNode b) Nothing Nothing
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
(-->)
  :: (ToEdgeNode a, ToEdgeNode b, MonadDot m)
  => a
  -> b
  -> m Entity
(-->) = edge

-- | Creates a subgraph in the given context.
--
-- The newly created subgraph will be assigned all of the default 'Subgraph'
-- attributes (see 'defaults'). The argument to this function is an action in
-- the monad: all entities created in this action will be added to this new
-- subgraph.
--
-- This function updates the 'its' entity to this node *twice*: before executing
-- the callback, and before returning.
--
-- > graph do
-- >   subgraph_ do
-- >     its fontcolor ?= "green" -- points to the subgraph
-- >     x <- node "x"
-- >     its fontcolor ?= "red"   -- points to node "x"
-- >     pure x
-- >   use (its fontcolor)        -- points to the subgraph, returns green
--
-- The resulting monadic action will return the value return by the given action.
subgraph :: MonadDot m => m a -> m a
subgraph = recurse Subgraph

-- | Like 'subgraph', but ignores the result of the nested action.
subgraph_ :: MonadDot m => m a -> m ()
subgraph_ = void . recurse Subgraph

-- | Like 'subgraph', but creates a cluster instead.
--
-- The created entity will use the default 'Cluster' attributes.
cluster :: MonadDot m => m a -> m a
cluster = recurse Cluster

-- | Like 'cluster', but ignores the result of the nested action.
cluster_ :: MonadDot m => m a -> m ()
cluster_ = void . recurse Cluster

-- | Associate the given entity to the given name.
--
-- The 'DotT' monad will store an association from the name to the entity,
-- allowing edges to be declared by making reference to that name using
-- 'Text.Dot.retrieve'.
--
-- > digraph do
-- >   cluster_ do
-- >     cluster_ do
-- >       cluster_ do
-- >         cluster_ do
-- >           a <- node "A"
-- >           register a "the A node"
-- >   b <- node "B"
-- >   edge (retrieve "the A node") b
--
-- See also 'Text.Dot.edge'.
register :: MonadDot m => Entity -> Text -> m ()
register entity name = do
  entityRegister %= M.insert name entity

-- | Associate the latest entity to the given name.
--
-- Like 'register', but uses the last created entity.
registerItAs :: MonadDot m => Text -> m ()
registerItAs name = do
  currentEntity <- itsID
  register currentEntity name



--------------------------------------------------------------------------------
-- Internal helpers

recurse :: MonadDot m => EntityType -> m a -> m a
recurse etype action = do
  entity <- record etype
  contextStack %= NE.cons mempty
  result <- withPath entity action
  sub <- popContext
  subgraphInfo . at entity ?= sub
  latest .= entity
  pure result

record :: MonadDot m => EntityType -> m Entity
record etype = do
  suffix <- use entityIndex
  let entity = Entity etype suffix
  defAttrs <- use $ defaultAttributes . at etype . non mempty
  context <>:= [entity]
  entityIndex += 1
  attributes entity .= defAttrs
  latest .= entity
  pure entity
