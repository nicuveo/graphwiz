module Reporter (customReporter) where

import "this" Prelude

import Control.Concurrent.STM qualified as STM
import Data.IntMap            qualified as IM
import Data.Maybe             (mapMaybe)
import Data.Proxy
import Data.Tagged
import Data.Text              qualified as T
import Data.Text.IO           qualified as T
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Runners
import Text.Builder           qualified as TB


-- custom CI reporter

customReporter :: String -> Ingredient
customReporter = TestReporter [Option (Proxy :: Proxy PathPrefix)] . mkRunner


-- option

newtype PathPrefix = PathPrefix FilePath
  deriving (IsString)

instance IsOption PathPrefix where
  defaultValue = "report"
  parseValue = Just . PathPrefix
  optionName = Tagged "report"
  optionHelp = Tagged "prefix for output files"


-- internal monad

newtype FoldMonad m a = FoldMonad (StateT Int m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState Int
           , MonadIO
           )

runFold :: Monad m => FoldMonad m a -> m a
runFold (FoldMonad m) = evalStateT m 0

getIndex :: Monad m => FoldMonad m Int
getIndex = get <* modify (+1)


-- cursed
--
-- a monoid instance is required, but not actually used, since we run our own
-- custon fold. `foldTestTree` just uses mempty once. so we make those weird
-- instances just for compatibility

instance (Monad m, Semigroup a) => Semigroup (FoldMonad m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, Monad m) => Monoid (FoldMonad m a) where
  mempty = pure mempty


-- runner implemetation

mkRunner :: String -> OptionSet -> TestTree -> Maybe (StatusMap -> IO (Time -> IO Bool))
mkRunner suiteName options testTree =
  pure \statusMap -> do
    let PathPrefix prefix = lookupOption options
        summaryFile = prefix ++ "-summary.md"
        failingFile = prefix ++ "-failing.md"
    createDirectoryIfMissing True =<< canonicalizePath (takeDirectory prefix)
    result <- runFold $ foldTestTree (customFold statusMap) options testTree
    pure \timeElapsed -> do
      let
        total   = length result
        failure = length $ mapMaybe snd result
        success = total - failure
        summary = TB.intercalate " | "
          [ TB.string suiteName
          , TB.decimal total
          , TB.decimal success
          , TB.decimal failure
          , TB.decimal @Int $ round $ timeElapsed * 1000
          ]
        failing = TB.intercalate "\n" do
          (path, Just desc) <- result
          pure $ mconcat
            [ "<tr><td>"
            , TB.intercalate " &gt; " $ map TB.text path
            , "</td><td>"
            , TB.text desc
            , "</td></tr>"
            ] <> "\n"
      T.writeFile summaryFile $ TB.run ("| " <> summary <> " |\n")
      T.writeFile failingFile $ TB.run failing
      pure $ failure == 0

customFold :: StatusMap -> TreeFold (FoldMonad IO [([Text], Maybe Text)])
customFold status = trivialFold
  { foldSingle   = visitTest
  , foldGroup    = visitGroup
  }
  where
    visitTest :: forall t. OptionSet -> TestName -> t -> FoldMonad IO [([Text], Maybe Text)]
    visitTest _options testName _test = do
      index <- getIndex
      description <- liftIO $ readResult $ status IM.! index
      pure $ pure ([T.pack testName], description)

    visitGroup _options groupName actions = do
      results <- concat <$> sequence actions
      pure do
        (testPath, testDesc) <- results
        pure (T.pack groupName : testPath, testDesc)

readResult :: STM.TVar Status -> IO (Maybe Text)
readResult var =
  STM.atomically $
    STM.readTVar var >>= \case
      Done result -> pure $ getDescription result
      _           -> STM.retry

getDescription :: Result -> Maybe Text
getDescription result = case resultOutcome result of
  Success                        -> Nothing
  Failure (TestTimedOut       _) -> Just "TimeOut"
  Failure (TestThrewException e) -> Just (T.pack $ show e)
  _                              -> Just (T.pack $ resultDescription result)
