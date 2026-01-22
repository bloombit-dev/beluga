{-# LANGUAGE DuplicateRecordFields #-}

module Binja.ControlFlowGraph
  ( Binja.ControlFlowGraph.CFG,
    Binja.ControlFlowGraph.create,
  )
where

import Binja.BasicBlock
import Binja.Function
import Binja.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

data Vertex = Vertex
  { block :: Binja.Types.BasicBlockMlilSSA,
    edge :: Binja.Types.BasicBlockEdge
  }
  deriving (Show, Eq, Ord)

type CFG = Map.Map Vertex (Set.Set Vertex)

-- outgoing edges becomes a lookup into the graph
--
-- Note: this could be done once during creation and cached
-- incoming edges becomces filter of the graph of keys with children containing block of interest
--

create :: FunctionContext -> IO CFG
create context = do
  rawBlocks <- Binja.BasicBlock.fromMlilSSAFunction $ handle context
  liftedBlocks <- mapM Binja.BasicBlock.fromBlockPtr rawBlocks
  rawOutgoingEdges <- mapM Binja.BasicBlock.outgoingEdges rawBlocks
  -- lift the target basic block to the higher level type
  let outgoingEdges' = Prelude.map (Prelude.map Binja.BasicBlock.fromBlockEdge) rawOutgoingEdges
  -- construct the CFG:
  --   (1) each liftedBlock is an entry into the CFG
  --   (2) children are derived from (targets of rawOutgoingEdges, outgoingEdges')
  pure Map.empty
