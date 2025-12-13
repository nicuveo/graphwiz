module Text.Dot.Render
  ( graphWithT
  , graphT
  , graphWith
  , graph
  , digraphWithT
  , digraphT
  , digraphWith
  , digraph
  , strictGraphWithT
  , strictGraphT
  , strictGraphWith
  , strictGraph
  , strictDigraphWithT
  , strictDigraphT
  , strictDigraphWith
  , strictDigraph
  ) where

import "this" Prelude

import Data.HashMap.Strict qualified as M
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
-- underlying monad, as a 'TextBuilder'. The callback takes the graph's
-- identifier as argument.
--
-- The result of the graph building expression itself is ignored.
graphWithT :: Monad m => (Entity -> DotT m a) -> m TextBuilder
graphWithT = render "graph" "--"

-- | Renders a given graph.
--
-- Like 'graphWithT', but the expression doesn't take the identifier as agument.
graphT :: Monad m => DotT m a -> m TextBuilder
graphT = render "graph" "--" . const

-- | Renders a given graph.
--
-- Like 'graphWithT', but in the 'Dot' monad.
graphWith :: (Entity -> Dot a) -> TextBuilder
graphWith = runIdentity . render "graph" "--"

-- | Renders a given graph.
--
-- Like 'graphT', but in the 'Dot' monad.
graph :: Dot a -> TextBuilder
graph = runIdentity . render "graph" "--" . const

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds a directed non-strict graph. It returns the result in the
-- underlying monad, as a 'TextBuilder'. The callback takes the graph's
-- identifier as argument.
--
-- The result of the graph building expression itself is ignored.
digraphWithT :: Monad m => (Entity -> DotT m a) -> m TextBuilder
digraphWithT = render "digraph" "->"

-- | Renders a given graph.
--
-- Like 'digraphWithT', but the expression doesn't take the entity as agument.
digraphT :: Monad m => DotT m a -> m TextBuilder
digraphT = render "digraph" "->" . const

-- | Renders a given graph.
--
-- Like 'digraphWithT', but in the 'Dot' monad.
digraphWith :: (Entity -> Dot a) -> TextBuilder
digraphWith = runIdentity . render "digraph" "->"

-- | Renders a given graph.
--
-- Like 'digraphT', but in the 'Dot' monad.
digraph :: Dot a -> TextBuilder
digraph = runIdentity . render "digraph" "->" . const

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds an undirected strict graph. It returns the result in the
-- underlying monad, as a 'TextBuilder'. The callback takes the graph's
-- identifier as argument.
--
-- The result of the graph building expression itself is ignored.
strictGraphWithT :: Monad m => (Entity -> DotT m a) -> m TextBuilder
strictGraphWithT = render "strict graph" "--"

-- | Renders a given graph.
--
-- Like 'strictGraphWithT', but the expression doesn't take the entity as agument.
strictGraphT :: Monad m => DotT m a -> m TextBuilder
strictGraphT = render "strict graph" "--" . const

-- | Renders a given graph.
--
-- Like 'strictGraphWithT', but in the 'Dot' monad.
strictGraphWith :: (Entity -> Dot a) -> TextBuilder
strictGraphWith = runIdentity . render "strict graph" "--"

-- | Renders a given graph.
--
-- Like 'strictGraphT', but in the 'Dot' monad.
strictGraph :: Dot a -> TextBuilder
strictGraph = runIdentity . render "strict graph" "--" . const

-- | Renders a given graph.
--
-- Given a t'DotT' expression that builds a graph, this function evaluates it
-- and builds a directed strict graph. It returns the result in the underlying
-- monad, as a 'TextBuilder'. The callback takes the graph's identifier as
-- argument.
--
-- The result of the graph building expression itself is ignored.
strictDigraphWithT :: Monad m => (Entity -> DotT m a) -> m TextBuilder
strictDigraphWithT = render "strict digraph" "->"

-- | Renders a given graph.
--
-- Like 'strictDigraphWithT', but the expression doesn't take the entity as agument.
strictDigraphT :: Monad m => DotT m a -> m TextBuilder
strictDigraphT = render "strict digraph" "->" . const

-- | Renders a given graph.
--
-- Like 'strictDigraphWithT', but in the 'Dot' monad.
strictDigraphWith :: (Entity -> Dot a) -> TextBuilder
strictDigraphWith = runIdentity . render "strict digraph" "->"

-- | Renders a given graph.
--
-- Like 'strictDigraphT', but in the 'Dot' monad.
strictDigraph :: Dot a -> TextBuilder
strictDigraph = runIdentity . render "strict digraph" "->" . const


--------------------------------------------------------------------------------
-- Internal helpers

render :: Monad m => TextBuilder -> TextBuilder -> (Entity -> DotT m a) -> m TextBuilder
render gtype arrow f = do
  let root = Entity Subgraph (-1)
  allGraph <- run root (f root)
  pure $ TB.intercalate "\n" $ visit allGraph gtype arrow root

indent :: [TextBuilder] -> [TextBuilder]
indent = map ("  " <>)

visit :: DotGraph -> TextBuilder -> TextBuilder -> Entity -> [TextBuilder]
visit DotGraph {..} gtype arrow = visitGraph
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

    visitAttribute (name, value) =
      mconcat [TB.text name, "=\"", TB.text value, "\""]

    visitAttributes =
      map visitAttribute . M.toList

    visitEntity e =
      let attrs = fromMaybe mempty $ M.lookup e _entityAttributes
      in  indent $ case getType e of
                     Node     -> visitNode     e attrs
                     Edge     -> visitEdge     e attrs (_edgeInfo     M.! e)
                     Cluster  -> visitSubgraph e attrs (_subgraphInfo M.! e)
                     Subgraph -> visitSubgraph e attrs (_subgraphInfo M.! e)

    visitEntities =
      concatMap visitEntity . reverse

    visitNode e attrs =
      pure $ mconcat
        [ renderIndex e
        , " ["
        , TB.intercalate "," $ visitAttributes attrs
        , "]"
        ]

    visitEdge _ attrs (EdgeInfo o1 o2 p1 p2) =
      pure $ mconcat
        [ renderIndex p1
        , " "
        , arrow
        , " "
        , renderIndex p2
        , " ["
        , TB.intercalate "," $ visitAttributes $ attrs
          <> M.fromList [("ltail", TB.toText $ renderIndex o1) | getType o1 == Cluster]
          <> M.fromList [("lhead", TB.toText $ renderIndex o2) | getType o2 == Cluster]
        , "]"
        ]

    visitGraph e =
      visitInner gtype e (fromMaybe mempty $ M.lookup e _entityAttributes) (NE.head _contextStack)

    visitSubgraph e =
      visitInner ("subgraph " <> renderIndex e) e

    visitInner etype _ attrs entities = concat
      [ [etype <> " {"]
      , indent $ map (<> ";") $ visitAttributes attrs
      , visitEntities entities
      , ["}"]
      ]
