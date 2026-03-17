module Text.Dot.Monad where

import "this" Prelude

import Control.Lens
import Control.Monad.Writer
import Data.HashMap.Strict  qualified as M
import Data.List.NonEmpty   qualified as NE
import Data.Text            qualified as T

import Text.Dot.Attributes
import Text.Dot.Types


--------------------------------------------------------------------------------
-- Dot monad

-- | Dot creation monad.
newtype DotT m a = DotT (DotGraph -> Path -> m (a, DotGraph))
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Path
    , MonadState  DotGraph
    , MonadIO
    , MonadWriter w
    , MonadError  e
    ) via (StateT DotGraph (ReaderT Path m))

instance MonadTrans DotT where
  lift x = DotT \s _ -> fmap (,s) x

-- | An alias for @DotT Identity@.
type Dot = DotT Identity

-- | The constraint that all functions require.
--
-- We choose to express this as a constraint rather than a typeclass
-- for simplicity.
type MonadDot m = (MonadState DotGraph m, MonadReader Path m)

run :: Monad m => DotT m a -> m DotGraph
run action =
  let DotT f = action >> postProcess
  in  snd <$> f initialGraph (Path $ pure rootGraph)


--------------------------------------------------------------------------------
-- State manipulation

-- | Retrieve the current path.
--
-- The path is the stack of entities, representing the graph /
-- subgraphs / clusters between the root of the graph and the current
-- location.
--
-- > graph do
-- >   p1 <- currentPath     -- returns [$graphID]
-- >   subgraph do
-- >     cluster do
-- >       p2 <- currentPath -- returns [$clusterID, $subgraphID, $graphID]
-- >       doStuff
currentPath :: MonadDot m => m (NonEmpty Entity)
currentPath = asks unwrapPath

-- | Retrieves the unique ID of the last created t'Entity'.
itsID :: MonadDot m => m Entity
itsID = use latest

withPath :: MonadDot m => Entity -> m a -> m a
withPath e = local (_Path <>:~ pure e)

context :: Lens' DotGraph DotContext
context f d = fmap go (f c)
  where
    (c :| cs) = d ^. contextStack
    go nc = d & contextStack .~ nc :| cs

popContext :: MonadDot m => m DotContext
popContext = do
  c <- use context
  contextStack %= NE.fromList . NE.tail
  pure c

postProcess :: Monad m => DotT m ()
postProcess = do
  fixedEdges <- traverse fixEdge =<< use edgeInfo
  edgeInfo .= fixedEdges
  where
    fixEdge (EdgeInfo a b _ _) = do
      ea <- resolve a
      eb <- resolve b
      na <- getEnd ea
      nb <- getEnd eb
      pure $ EdgeInfo (KnownNode ea) (KnownNode eb) na nb

    resolve = \case
      KnownNode   eid  -> pure eid
      UnknownNode name -> dereference name

    getEnd eid =
      case getType eid of
        Cluster -> do
          attributes rootGraph . compound ?= "true"
          locateNode eid
        _ -> pure Nothing

    locateNode eid = do
      dg <- get
      pure $ eid ^? visit dg

    visit dg f eid =
      case getType eid of
        Cluster  -> foldMapOf (subgraphInfo . at eid . traverse . traverse) (visit dg f) dg
        Subgraph -> foldMapOf (subgraphInfo . at eid . traverse . traverse) (visit dg f) dg
        Node     -> f eid
        Edge     -> mempty

dereference :: Monad m => Text -> DotT m Entity
dereference name = do
  uses entityRegister (M.lookup name) >>= \case
    Just eid -> pure eid
    Nothing -> do
      error $ concat
        [ "Text.Dot.Monad.retrieve: unknown entity \""
        , T.unpack name
        , "\"\nall names must be registered via a call to `register` or `registerItAs`"
        ]
