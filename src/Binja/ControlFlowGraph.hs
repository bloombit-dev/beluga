{-# LANGUAGE DuplicateRecordFields #-}

module Binja.ControlFlowGraph
  ( Binja.ControlFlowGraph.create,
    Binja.ControlFlowGraph.blocks,
    Binja.ControlFlowGraph.order,
    Binja.ControlFlowGraph.size,
    Binja.ControlFlowGraph.contains,
  )
where

import Binja.BasicBlock
import Binja.Function (mlilToRawFunction, start)
import Binja.Types.Core (BNMlilSSAFunctionPtr, BasicBlockMlilSSA (..), CFGContext (..), Word64)
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

create :: BNMlilSSAFunctionPtr -> IO Binja.Types.Core.CFGContext
create handle' = do
  -- blocks in function
  rawBlocks <- Binja.BasicBlock.fromMlilSSAFunction handle'
  liftedBlocks <- mapM (Binja.BasicBlock.fromBlockPtr handle') rawBlocks
  -- entry block
  rawFunctionPtr <- mlilToRawFunction handle'
  startFunctionAddress <- start rawFunctionPtr
  entryBlock' <-
    case Data.List.find ((startFunctionAddress ==) . startAddress) liftedBlocks of
      Nothing -> error "Binja.ControlFlowGraph.create: No entry block found."
      Just bb -> pure bb
  -- edges from blocks
  rawOutgoingEdges <- mapM Binja.BasicBlock.outgoingEdges rawBlocks
  outgoingEdges' <- mapM (mapM (Binja.BasicBlock.fromBlockEdge handle')) rawOutgoingEdges
  let graph' =
        Map.fromList $
          zipWith (\vertex edge -> (vertex, Set.fromList edge)) liftedBlocks outgoingEdges'
  pure $ Binja.Types.Core.CFGContext {entry = entryBlock', graph = graph'}

-- | List of blocks making up function
blocks :: Binja.Types.Core.CFGContext -> [BasicBlockMlilSSA]
blocks = Map.keys . graph

-- | Number of nodes
order :: Binja.Types.Core.CFGContext -> Int
order = Map.size . graph

-- | Numer of edges
size :: Binja.Types.Core.CFGContext -> Int
size = sum . map Set.size . Map.elems . graph

-- | True if any block in the control flow graph contains an address
contains :: Binja.Types.Core.CFGContext -> Word64 -> Bool
contains cfg address = any (address ==) $ map startAddress $ blocks cfg
