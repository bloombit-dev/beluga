{-# LANGUAGE DuplicateRecordFields #-}

module Callgraph
  ( Callgraph.create,
    Callgraph.Graph,
    Callgraph.Vertex,
    --    Callgraph.vertices,
    --    Callgraph.neighbors,
    --    Callgraph.filter,
    --    Callgraph.recursive,
    --    Callgraph.leaf,
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

empty :: Graph
empty = Map.empty

create :: BNBinaryViewPtr -> IO Graph
create view = do
  funcs <- Binja.BinaryView.functions view
  mlilFuncs <- mapM Binja.Function.mlil funcs
  mlilInstructions <- mapM Binja.Mlil.instructionsFromFunc mlilFuncs
  putStrLn ("Number of functions: " ++ show (length funcs))
  let calls = map (filter isCall) mlilInstructions
  children' <- mapM (mapM (extractCallDestSymbol view)) calls
  parents <- mapM Binja.Function.symbol funcs
  pure $ Map.fromList $ zip parents $ map (Set.fromList . catMaybes) children'
  where
    isCall :: MediumLevelILSSAInstruction -> Bool
    isCall (Localcall _) = True
    isCall (Tailcall _) = True
    isCall (Syscall _) = True
    isCall _ = False
