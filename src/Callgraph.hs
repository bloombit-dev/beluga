{-# LANGUAGE DuplicateRecordFields #-}

module Callgraph
  ( Callgraph.create,
    Callgraph.Graph,
    Callgraph.Vertex,
    Callgraph.vertices,
    Callgraph.neighbors,
    Callgraph.filter,
    Callgraph.filterWithKey,
    Callgraph.recursive,
    Callgraph.leaf,
    Callgraph.callers,
    Callgraph.callees,
    Callgraph.mostCalled,
    Callgraph.mostConnected,
    Callgraph.reachable,
    Callgraph.order,
    Callgraph.size,
  )
where

import Binja.AnalysisContext
import Binja.Mlil
import Binja.Types
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

type Vertex = Binja.Types.Symbol

type Graph = Map.Map Vertex (Set.Set Vertex)

create :: AnalysisContext -> Graph
create context =
  Map.union initialGraph $ Map.fromList $ Prelude.map (\v -> (v, Set.empty)) allChildren
  where
    initialGraph :: Graph
    initialGraph =
      Map.fromList $
        map (\f -> (symbol f, Binja.AnalysisContext.callers context f)) $
          functions context
    allChildren :: [Vertex]
    allChildren = Set.toList $ Set.unions $ Map.elems initialGraph

vertices :: Graph -> [Vertex]
vertices = Map.keys

neighbors :: Graph -> Vertex -> Maybe (Set.Set Vertex)
neighbors graph source = Map.lookup source graph

-- Filter all children that satisfy the predicate
filter :: (Set.Set Vertex -> Bool) -> Graph -> Graph
filter = Map.filter

-- Filter all keys/values that satisfy the predicate
filterWithKey :: (Vertex -> Set.Set Vertex -> Bool) -> Graph -> Graph
filterWithKey = Map.filterWithKey

-- List of recursive symbols
recursive :: Graph -> [Vertex]
recursive graph = Callgraph.vertices $ Callgraph.filterWithKey (\parent child -> Set.member parent child) graph

-- List of symbols with no
leaf :: Graph -> [Vertex]
leaf graph = Callgraph.vertices $ Callgraph.filter Set.null graph

-- List of symbols which call source symbol
callers :: Graph -> Vertex -> [Vertex]
callers graph source = Callgraph.vertices $ Callgraph.filter (Set.member source) graph

-- List of symbols which source symbol calls
callees :: Graph -> Vertex -> [Vertex]
callees graph source =
  maybe [] Set.toList $ Callgraph.neighbors graph source

mostCalled :: Graph -> Maybe Vertex
mostCalled graph =
  case Callgraph.vertices graph of
    [] -> Nothing
    v : vs -> Just $ fst $ foldr step (v, value v) vs
  where
    value :: Vertex -> Int
    value v = length (Callgraph.callers graph v)

    step :: Vertex -> (Vertex, Int) -> (Vertex, Int)
    step candidate (curVertex, curVal) =
      if curVal < value candidate
        then (candidate, value candidate)
        else (curVertex, curVal)

mostConnected :: Graph -> Maybe Vertex
mostConnected graph =
  case Callgraph.vertices graph of
    [] -> Nothing
    v : vs -> Just $ fst $ foldr step (v, value v) vs
  where
    value :: Vertex -> Int
    value v =
      length (Callgraph.callers graph v)
        + length (callees graph v)

    step :: Vertex -> (Vertex, Int) -> (Vertex, Int)
    step candidate (curVertex, curVal) =
      if curVal < value candidate
        then (candidate, value candidate)
        else (curVertex, curVal)

-- is destination node reachable in source
reachable :: Graph -> Vertex -> Vertex -> Bool
reachable graph source destination = go Set.empty source
  where
    go :: Set.Set Vertex -> Vertex -> Bool
    go visited v
      | v == destination = True
      | Set.member v visited = False
      | otherwise =
          case Callgraph.neighbors graph v of
            Nothing -> False
            Just ns ->
              let visited' = Set.insert v visited
               in any (go visited') (Set.toList ns)

-- Number of nodes
order :: Graph -> Int
order = Map.size

-- Numer of edges
size :: Graph -> Int
size = sum . map Set.size . Map.elems
