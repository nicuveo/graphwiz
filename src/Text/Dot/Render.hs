module Text.Dot.Render
  ( graphT
  , graph
  , digraphT
  , digraph
  , strictGraphT
  , strictGraph
  , strictDigraphT
  , strictDigraph
  ) where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict qualified as M
import Data.HashSet        qualified as S
import Data.List.NonEmpty  qualified as NE
import Text.Printf
import TextBuilder         (TextBuilder)
import TextBuilder         qualified as TB

import Text.Dot.Monad
import Text.Dot.Types


--------------------------------------------------------------------------------
-- Render functions

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds an undirected non-strict graph. It returns the result in the
-- underlying monad, as a 'TextBuilder'.
--
-- The result of the graph building expression itself is ignored.
graphT :: Monad m => DotT m a -> m TextBuilder
graphT = render "graph" "--"

-- | Renders a given graph.
--
-- Like 'graphT', but in the 'Dot' monad.
graph :: Dot a -> TextBuilder
graph = runIdentity . render "graph" "--"

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds a directed non-strict graph. It returns the result in the
-- underlying monad, as a 'TextBuilder'.
--
-- The result of the graph building expression itself is ignored.
digraphT :: Monad m => DotT m a -> m TextBuilder
digraphT = render "digraph" "->"

-- | Renders a given graph.
--
-- Like 'digraphT', but in the 'Dot' monad.
digraph :: Dot a -> TextBuilder
digraph = runIdentity . render "digraph" "->"

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds an undirected strict graph. It returns the result in the
-- underlying monad, as a 'TextBuilder'.
--
-- The result of the graph building expression itself is ignored.
strictGraphT :: Monad m => DotT m a -> m TextBuilder
strictGraphT = render "strict graph" "--"

-- | Renders a given graph.
--
-- Like 'strictGraphT', but in the 'Dot' monad.
strictGraph :: Dot a -> TextBuilder
strictGraph = runIdentity . render "strict graph" "--"

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds a directed strict graph. It returns the result in the underlying
-- monad, as a 'TextBuilder'.
--
-- The result of the graph building expression itself is ignored.
strictDigraphT :: Monad m => DotT m a -> m TextBuilder
strictDigraphT = render "strict digraph" "->"

-- | Renders a given graph.
--
-- Like 'strictDigraphT', but in the 'Dot' monad.
strictDigraph :: Dot a -> TextBuilder
strictDigraph = runIdentity . render "strict digraph" "->"


--------------------------------------------------------------------------------
-- Internal helpers

render :: Monad m => TextBuilder -> TextBuilder -> DotT m a -> m TextBuilder
render gtype arrow action = do
  allGraph <- run action
  pure $ TB.intercalate "\n" $ visit allGraph gtype arrow

indent :: [TextBuilder] -> [TextBuilder]
indent = map ("  " <>)

visit :: DotGraph -> TextBuilder -> TextBuilder -> [TextBuilder]
visit DotGraph {..} gtype arrow = evalState visitGraph initialRenderState
  where
    magnitude = ceiling (logBase 10 (fromIntegral _entityIndex :: Double)) :: Int
    intFormat = mconcat ["%0", show magnitude, "d"]

    renderIndex (Entity t i) =
      let prefix = case t of
            Subgraph -> "subgraph"
            Cluster  -> "cluster"
            Node     -> "node"
            Edge     -> error "Text.Dot.Render.visit: tried to render an edge id"
          suffix = TB.string $ printf intFormat i
      in prefix <> suffix

    retrieveEdgeNode = \case
      KnownNode e -> e
      UnknownNode _ -> error "Text.Dot.Render.visit: found unresolved edge name"

    visitAttribute (name, value) =
      mconcat [TB.text name, "=\"", TB.text value, "\""]

    visitAttributes =
      map visitAttribute . M.toList

    visitEntity e = do
      let attrs = fromMaybe mempty $ M.lookup e _entityAttributes
      case getType e of
        Node     -> visitNode     e attrs
        Edge     -> visitEdge     e attrs (_edgeInfo     M.! e)
        Cluster  -> visitSubgraph e attrs (_subgraphInfo M.! e)
        Subgraph -> visitSubgraph e attrs (_subgraphInfo M.! e)

    visitEntities =
      fmap concat . traverse visitEntity . reverse

    visitNode e attrs = do
      knownNodes %= S.insert e
      pure $ pure $ mconcat
        [ renderIndex e
        , " ["
        , TB.intercalate "," $ visitAttributes attrs
        , "]"
        ]

    visitEdge _ attrs (EdgeInfo o1 o2 p1 p2) = do
      let
        e1 = retrieveEdgeNode o1
        e2 = retrieveEdgeNode o2
        result = pure $ mconcat
          [ renderIndex (fromMaybe e1 p1)
          , " "
          , arrow
          , " "
          , renderIndex (fromMaybe e2 p2)
          , " ["
          , TB.intercalate "," $ visitAttributes $ attrs
            <> M.fromList [("ltail", TB.toText $ renderIndex e1) | _ <- toList p1]
            <> M.fromList [("lhead", TB.toText $ renderIndex e2) | _ <- toList p2]
          , "]"
          ]
      knownCache <- use knownNodes
      let allKnown = and do
            Just n <- [Just e1, Just e2, p1, p2]
            pure $ S.member n knownCache
      if allKnown
      then
        pure result
      else do
        delayedEdges <>:= result
        pure []

    visitGraph = visitInner
      gtype
      (uses delayedEdges reverse)
      (fromMaybe mempty $ M.lookup rootGraph _entityAttributes)
      (NE.head _contextStack)

    visitSubgraph e = visitInner
      ("subgraph " <> renderIndex e)
      (pure [])

    visitInner etype edgeAction attrs entities = do
      innerEntities <- visitEntities entities
      extraEdges <- edgeAction
      pure $ concat
        [ [etype <> " {"]
        , indent $ map (<> ";") $ visitAttributes attrs
        , indent innerEntities
        , indent extraEdges
        , ["}"]
        ]
