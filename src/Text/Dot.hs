{- |

GraphWiz is a small monadic DSL used to write DOT files.

To run the monad, use one of the [graph functions](#g:render), like 'graph' and
'digraph'. Those also have a transformer version if you want to use 'Dot' on top
of other monads.

Within the monad, you can use any of the [construction](#g:construction)
functions to create one of the four graph [entities](#g:entities). Their
attributes can be set via lenses such as '?=', see [attributes](#g:attributes).

The output is a 'Text.Builder.Builder', that you can convert to a strict
'Data.Text.Text' or print directly.

-}

module Text.Dot
  ( -- * Entities #entities#
    Entity
  , EntityType (..)
  , getType
  , itsID
    -- * Attributes #attributes#
  , Attributes
  , attributes
  , defaults
  , attribute
  , its
  , ifAbsent
    -- * Construction #construction#
  , node
  , edge
  , (-->)
  , subgraphWith
  , subgraph
  , subgraphWith_
  , subgraph_
  , clusterWith
  , cluster
  , clusterWith_
  , cluster_
    -- * Monad #monad#
  , DotT
  , Dot
  , MonadDot
  , DotGraph
  , Path
  , path
  , rootGraph
    -- * Rendering the graph #render#
  , module Render
    -- * Re-exports from "Control.Lens.Setter"
  , (.=)
  , (?=)
  , (%=)
  , (<>=)
  , (<>:=)
    -- * All known attributes
    -- ** Renamed attributes
    -- $attributes
  , background
  , damping
  , isCcluster
  , k
  , svgClass
  , svgID
  , tbbalance
  , url
    -- ** All others #hell#
  , module Attributes
  ) where

import Control.Lens.Setter

import Text.Dot.Attributes (attribute, attributes, background, damping,
                            defaults, ifAbsent, isCcluster, its, k, svgClass,
                            svgID, tbbalance, url)
import Text.Dot.Attributes as Attributes hiding (attribute, attributes,
                                          background, damping, defaults,
                                          ifAbsent, isCcluster, its, k,
                                          svgClass, svgID, tbbalance, url)
import Text.Dot.Build
import Text.Dot.Monad
import Text.Dot.Render     as Render
import Text.Dot.Types

-- $attributes
--
-- All attributes listed in [Graphviz's
-- documentation](https://graphviz.org/doc/info/attrs.html) have an accompanying
-- lens, so that any standard attribute can be accessed without using strings
-- (like with 'attribute'). Not all of them are valid haskell names, however:
-- the following are the ones that have been remamed, the others ones can be
-- found [below](#g:hell).
