module Text.Dot.Monad where

import "this" Prelude

import Control.Lens
import Data.List.NonEmpty qualified as NE

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
    ) via (StateT DotGraph (ReaderT Path m))

-- | An alias for @DotT Identity@.
type Dot = DotT Identity

-- | The constraint that all Dot functions require.
--
-- We choose to express this as a constraint rather than a typeclass
-- for simplicity.
type MonadDot m = (MonadState DotGraph m, MonadReader Path m)

run :: Monad m => Entity -> DotT m a -> m DotGraph
run e (DotT f) = snd <$> f (initialGraph e) (Path $ pure e)


--------------------------------------------------------------------------------
-- State manipulation

-- | Retrieve the current path.
--
-- The path is the stack of entities, representing the graph /
-- subgraphs / clusters between the root of the graph and the current
-- location.
--
-- @
--    graph do
--      p1 <- path     -- returns []
--      subgraph do
--        cluster do
--          p2 <- path -- returns [clusterEntity, subgraphEntity]
--          doStuff
-- @
path :: MonadDot m => m (NonEmpty Entity)
path = asks unwrapPath

-- | Retrieves the unique ID of the last created 'Entity'.
itsID :: MonadDot m => m Entity
itsID = use latest

-- | Retrieves the unique ID of the top-level graph.
rootGraph :: MonadDot m => m Entity
rootGraph = views _Path NE.last

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
