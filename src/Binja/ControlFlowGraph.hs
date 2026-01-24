{-# LANGUAGE DuplicateRecordFields #-}

module Binja.ControlFlowGraph
  ( Binja.ControlFlowGraph.create,
  )
where

import Binja.BasicBlock
import Binja.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

-- outgoing edges becomes a lookup into the graph
--
-- Note: this could be done once during creation and cached
-- incoming edges becomces filter of the graph of keys with children containing block of interest
--

create :: BNMlilSSAFunctionPtr -> IO Binja.Types.CFG
create handle' = do
  -- blocks in function
  rawBlocks <- Binja.BasicBlock.fromMlilSSAFunction handle'
  liftedBlocks <- mapM Binja.BasicBlock.fromBlockPtr rawBlocks
  -- edges from blocks
  rawOutgoingEdges <- mapM Binja.BasicBlock.outgoingEdges rawBlocks
  outgoingEdges' <- mapM (mapM Binja.BasicBlock.fromBlockEdge) rawOutgoingEdges
  -- construct control flow graph
  let initialGraph =
        Map.fromList $
          zipWith (\vertex edge -> (vertex, Set.fromList edge)) liftedBlocks outgoingEdges'
  let allChildren =
        Set.toList $
          Set.map (\(BasicBlockEdge {target = t}) -> t) $
            Set.unions $
              Map.elems initialGraph
  pure $
    Map.union initialGraph $
      Map.fromList $
        Prelude.map (\v -> (v, Set.empty)) allChildren
