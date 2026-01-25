{-# LANGUAGE DuplicateRecordFields #-}

module Binja.ControlFlowGraph
  ( Binja.ControlFlowGraph.create,
    Binja.ControlFlowGraph.blocks,
    Binja.ControlFlowGraph.order,
    Binja.ControlFlowGraph.size,
  )
where

import Binja.BasicBlock
import Binja.Types (BNMlilSSAFunctionPtr, BasicBlockEdge (..), BasicBlockMlilSSA (..), CFGContext (..))
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- outgoing edges becomes a lookup into the graph
--
-- Note: this could be done once during creation and cached
-- incoming edges becomces filter of the graph of keys with children containing block of interest
--

create :: BNMlilSSAFunctionPtr -> IO Binja.Types.CFGContext
create handle' = do
  -- blocks in function
  rawBlocks <- Binja.BasicBlock.fromMlilSSAFunction handle'
  liftedBlocks <- mapM Binja.BasicBlock.fromBlockPtr rawBlocks
  -- entry block
  entryBlock' <-
    case Data.List.find ((0 ==) . start) liftedBlocks of
      Nothing -> error "Binja.ControlFlowGraph.create: No entry block found."
      Just bb -> pure bb
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
  let graph' =
        Map.union initialGraph $
          Map.fromList $
            Prelude.map (\v -> (v, Set.empty)) allChildren
  pure $ Binja.Types.CFGContext {entry = entryBlock', graph = graph'}

blocks :: Binja.Types.CFGContext -> [BasicBlockMlilSSA]
blocks = Map.keys . graph

-- | Number of nodes
order :: Binja.Types.CFGContext -> Int
order = Map.size . graph

-- | Numer of edges
size :: Binja.Types.CFGContext -> Int
size = sum . map Set.size . Map.elems . graph
