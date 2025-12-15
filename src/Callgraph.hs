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
  )
where

import Binja.BinaryView
import Binja.Function
import Binja.Mlil
import Binja.Types
-- import Control.Parallel.Strategies
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

type Vertex = Binja.Types.Symbol

type Graph = Map.Map Vertex (Set.Set Vertex)

create :: BNBinaryViewPtr -> IO Graph
create view = do
  funcs <- Binja.BinaryView.functions view
  mlilFuncs <- mapM Binja.Function.mlil funcs
  mlilInstructions <- mapM Binja.Mlil.instructionsFromFunc mlilFuncs
  let calls = map (Prelude.filter isCall) mlilInstructions
  children' <- mapM (mapM (Binja.Mlil.extractCallDestSymbol view)) calls
  parents <- mapM Binja.Function.symbol funcs
  let graph' = Map.fromList $ zip parents $ map (Set.fromList . catMaybes) children'
  pure $
    let allChildren = Set.unions (Map.elems graph')
     in Set.foldr
          (\child -> Map.insertWith (\_ old -> old) child Set.empty)
          graph'
          allChildren
  where
    isCall :: MediumLevelILSSAInstruction -> Bool
    isCall (Localcall _) = True
    isCall (Tailcall _) = True
    isCall (Syscall _) = True
    isCall _ = False

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

-- Set of recursive symbols
recursive :: Graph -> [Vertex]
recursive graph = Callgraph.vertices $ Callgraph.filterWithKey (\parent child -> Set.member parent child) graph

-- Set of symbols with no
leaf :: Graph -> [Vertex]
leaf graph = Callgraph.vertices $ Callgraph.filter Set.null graph
