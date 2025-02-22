module Main where

{- AUTOCOLLECT.MAIN

group_type = tree
custom_main = True

-}

import "this" Prelude

import Data.List          (isPrefixOf)
import System.Environment
import Test.Tasty

import Reporter

{- AUTOCOLLECT.MAIN.imports -}

main :: IO ()
main = do
  args <- getArgs
  suiteName <- fromMaybe "graphwiz" <$> lookupEnv "TEST_SUITE_NAME"
  let tests = testGroup suiteName ({- AUTOCOLLECT.MAIN.tests -})
  if any ("--report" `isPrefixOf`) args
  then defaultMainWithIngredients [customReporter suiteName] tests
  else defaultMain tests
