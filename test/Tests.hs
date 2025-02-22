module Main where

{- AUTOCOLLECT.MAIN

group_type = tree
custom_main = True

-}

import "this" Prelude

import Data.List                 (isPrefixOf)
import System.Environment
import Test.Tasty
import Test.Tasty.Runners.AntXML

{- AUTOCOLLECT.MAIN.imports -}

main :: IO ()
main = do
  args <- getArgs
  suiteName <- lookupEnv "TEST_SUITE_NAME"
  let tests = testGroup (fromMaybe "graphwiz" suiteName) ({- AUTOCOLLECT.MAIN.tests -})
  if any ("--xml" `isPrefixOf`) args
  then defaultMainWithIngredients [antXMLRunner] tests
  else defaultMain tests
