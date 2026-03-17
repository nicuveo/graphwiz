{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedLists #-}

module Golden where

import "this" Prelude

import Control.Lens
import Data.Text.Lazy          qualified as T
import Data.Text.Lazy.Encoding qualified as T
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.Dot
import TextBuilder             (TextBuilder)
import TextBuilder             qualified as TB

go :: String -> TextBuilder -> TestTree
go testname = goldenVsString testname filename . pure . builderToBytestring
  where
    filename = "test" </> "golden" </> intercalate "_" (words testname) <> ".dot"
    builderToBytestring = T.encodeUtf8 . T.fromStrict . TB.toText . (<> "\n")

test =
  go "simple graph" $
    graph do
      a <- node "a"
      b <- node "b"
      c <- node "c"
      a --> b
      a --> b
      b --> c
      b --> c
      subgraph do
        d <- node "d"
        c --> d
        d --> a

test =
  go "digraph with attributes" $
    digraph do
      a <- node "a"
      its color ?= "red"
      its distortion %= ifAbsent "2"

      b <- node "b"
      c <- node "c"

      a --> b
      its style ?= "dotted"

      b --> c
      its color .= Just "green"

test =
  go "strict digraph with clusters" $
    strictDigraph do
      a <- cluster_ do
        its label ?= "cluster A"
        node "a"
      b <- cluster_ do
        its label ?= "cluster B"
        node "b"
      a --> b
      its style ?= "dotted"
      a --> b

test =
  go "automatic compound" $
    digraph do
      (clusterA1, _) <-
        cluster do
          its label ?= "cluster A1"
          cluster do
            its label ?= "cluster A2"
            cluster do
              its label ?= "cluster A3"
              node "a"
      (clusterB3, _) <-
        cluster_ do
          its label ?= "cluster B1"
          cluster_ do
            its label ?= "cluster B2"
            cluster do
              its label ?= "cluster B3"
              node "b"
      clusterA1 --> clusterB3

test =
  go "path test" $
    strictGraph do
      cluster do
        its label ?= "ROOT"
        makeNode
        cluster do
          its label ?= "L"
          makeNode
          cluster do
            its label ?= "L"
            makeNode
            cluster do
              its label ?= "L"
              makeNode
            cluster do
              its label ?= "R"
              makeNode
        cluster_ do
          its label ?= "R"
          makeNode
          cluster do
            its label ?= "L"
            makeNode
  where
    makeNode = do
      entityStack <- currentPath
      path <- sequence do
        eid <- toList entityStack
        pure $ use $ attributes eid . label
      node $ TB.toText $ TB.intercalate " > " $ reverse $ map TB.text $ catMaybes path

test =
  go "cyclic graph" $
    digraph do
      cluster_ do
        its label ?= "cluster A"
        registerItAs "cA"
        defaults Edge <>:= [("color", "red")]
        na <- node "node A"
        na --> retrieve "cB"
      cluster_ do
        its label ?= "cluster B"
        registerItAs "cB"
        defaults Edge <>:= [("color", "green")]
        nb <- node "node B"
        nb --> retrieve "cA"
      pure ()
