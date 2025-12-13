module Main where

import Control.Lens
import Data.Text.IO qualified as T
import Text.Dot
import TextBuilder  qualified as TB

main :: IO ()
main =
  T.putStrLn $ TB.toText $
    digraph do
      defaults Node . style ?= "filled"

      ast <- cluster_ do
        its label ?= "front end"

        source <- node "source code"
        its fillcolor ?= "#c3ffd8"

        ast <- node "AST"
        its fillcolor ?= "yellow"

        source --> ast
        its label ?= "parsing"

        pure ast

      cluster do
        its label ?= "middle end"

        ir <- node "IR"
        its shape   ?= "diamond"
        its fillcolor ?= "salmon"

        ast --> ir
        its label ?= "lowering"
        its style ?= "dotted"
