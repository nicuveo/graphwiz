# GraphWiz

GraphWiz provides a small monadic DSL to generate DOT files.

It's a "wizard" for Graphviz's DOT format, hence the name "GraphWiz".

<br />

## Example

##### Haskell source
```haskell
main =
  TB.putLnToStdOut $
    digraph do
      defaults Node style ?= "filled"

      ast <- cluster do
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
        its shape     ?= "diamond"
        its fillcolor ?= "salmon"

        ast --> ir
        its label ?= "lowering"
        its style ?= "dotted"
```

#### Resulting DOT file

```DOT
digraph {
  subgraph cluster0 {
    label="front end";
    node1 [label="source code",style="filled",fillcolor="#c3ffd8"]
    node2 [label="AST",style="filled",fillcolor="yellow"]
    node1 -> node2 [label="parsing"]
  }
  subgraph cluster4 {
    label="middle end";
    node5 [label="IR",style="filled",fillcolor="salmon",shape="diamond"]
    node2 -> node5 [label="lowering",style="dotted"]
  }
}
```

#### Resulting PNG

![example of a generated PNG](/example/output.png)

<br />
