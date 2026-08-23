{-# LANGUAGE DuplicateRecordFields #-}

module DependenceGraph
  ( DependenceGraph.create,
  )
where

-- TODO: use children for instructions that don't have an output?

import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.AdjacencyMap as AM
import Binja.Types

data Vertex
  = Inst Binja.Types.MediumLevelILSSAInstruction
  | SSAVar Binja.Types.BNSSAVariable
  deriving (Eq, Ord, Show)

type Graph = Acyclic.AdjacencyMap Vertex

data Dependence = Dependence
  { root :: Vertex,
    graph :: Graph
  }

create :: AnalysisContext -> MediumLevelILSSAInstruction -> Dependence
create context inst =
  Dependence
    { root = Inst inst,
      graph = createAux context inst Acyclic.empty
    }

-- | Add a single edge to the graph if the resulting graph is acyclic
addEdgeIfAcyclic :: Vertex -> Vertex -> Acyclic.AdjacencyMap Vertex -> Acyclic.AdjacencyMap Vertex
addEdgeIfAcyclic from to g =
  case Acyclic.toAcyclic candidate of
    Just g' -> g'
    Nothing -> g
  where
    candidate = AM.overlay (Acyclic.fromAcyclic g) (AM.edge from to)

-- | Fold a list of edges into a graph, dropping any that would create a cycle.
addEdgesIfAcyclic :: [(Vertex, Vertex)] -> Graph -> Graph
addEdgesIfAcyclic edges g0 = foldl (\g (from, to) -> addEdgeIfAcyclic from to g) g0 edges

addUnaryBase :: Vertex -> Vertex -> Graph -> Graph
addUnaryBase parent node graph' = addEdgeIfAcyclic parent node graph'

addBinaryBase :: Vertex -> Vertex -> Vertex -> Acyclic.AdjacencyMap Vertex -> Acyclic.AdjacencyMap Vertex
addBinaryBase parent left' right' graph' =
  addEdgeIfAcyclic parent left' $ addEdgeIfAcyclic parent right' graph'

createAux :: AnalysisContext -> MediumLevelILSSAInstruction -> Graph -> Graph
createAux context (Localcall lc) graph' =
  case lc of
    MediumLevelILCallSsa MediumLevelILCallSsaRec {output = o, dest = d, params = p} ->
      addEdgesIfAcyclic (outEdges o ++ paramEdges p) graph'
    MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec {output = o, dest = d, params = p} ->
      addEdgesIfAcyclic (outEdges o ++ paramEdges p) graph'
  where
    outEdges :: [BNSSAVariable] -> [(Vertex, Vertex)]
    outEdges = map (\node -> (Inst $ Localcall lc, SSAVar node))
    paramEdges :: [MediumLevelILSSAInstruction] -> [(Vertex, Vertex)]
    paramEdges = map (\node -> (Inst node, Inst $ Localcall lc))
createAux context (Constant _) graph' = graph'
createAux context (Comparison cmp) graph' =
  case cmp of
    MediumLevelILCmpE MediumLevelILCmpERec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpE MediumLevelILFcmpERec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpNe MediumLevelILCmpNeRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpNe MediumLevelILFcmpNeRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpLt MediumLevelILFcmpLtRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpLe MediumLevelILFcmpLeRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpGe MediumLevelILFcmpGeRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpGt MediumLevelILFcmpGtRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpSlt MediumLevelILCmpSltRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpUlt MediumLevelILCmpUltRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpSle MediumLevelILCmpSleRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpUle MediumLevelILCmpUleRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpSge MediumLevelILCmpSgeRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpUge MediumLevelILCmpUgeRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpSgt MediumLevelILCmpSgtRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILCmpUgt MediumLevelILCmpUgtRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpO MediumLevelILFcmpORec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFcmpUo MediumLevelILFcmpUoRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILTestBit MediumLevelILTestBitRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
  where
    parent :: Vertex
    parent = Inst (Comparison cmp)
createAux context (Arithmetic a) graph' =
  case a of
    MediumLevelILNeg MediumLevelILNegRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILNot MediumLevelILNotRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILSx MediumLevelILSxRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILZx MediumLevelILZxRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILLowPart MediumLevelILLowPartRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFsqrt MediumLevelILFsqrtRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFneg MediumLevelILFnegRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFabs MediumLevelILFabsRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFloatToInt MediumLevelILFloatToIntRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILIntToFloat MediumLevelILIntToFloatRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFloatConv MediumLevelILFloatConvRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILRoundToInt MediumLevelILRoundToIntRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFloor MediumLevelILFloorRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILCeil MediumLevelILCeilRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILFtrunc MediumLevelILFtruncRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILAdd MediumLevelILAddRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILSub MediumLevelILSubRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILAnd MediumLevelILAndRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILOr MediumLevelILOrRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILXor MediumLevelILXorRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILLsl MediumLevelILLslRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILLsr MediumLevelILLsrRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILAsr MediumLevelILAsrRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILRol MediumLevelILRolRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILRor MediumLevelILRorRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILMul MediumLevelILMulRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILDivu MediumLevelILDivuRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILDivs MediumLevelILDivsRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILModu MediumLevelILModuRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILMods MediumLevelILModsRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILAddOverflow MediumLevelILAddOverflowRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFadd MediumLevelILFaddRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFsub MediumLevelILFsubRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFmul MediumLevelILFmulRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILFdiv MediumLevelILFdivRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILBswap MediumLevelILBswapRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILPopcnt MediumLevelILPopcntRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILClz MediumLevelILClzRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILCtz MediumLevelILCtzRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILRbit MediumLevelILRbitRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILCls MediumLevelILClsRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
    MediumLevelILMins MediumLevelILMinsRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILMaxs MediumLevelILMaxsRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILMinu MediumLevelILMinuRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILMaxu MediumLevelILMaxuRec {left = l, right = r} ->
      addBinaryBase parent (Inst l) (Inst r) graph'
    MediumLevelILAbs MediumLevelILAbsRec {src = s} ->
      addUnaryBase parent (Inst s) graph'
  where
    parent :: Vertex
    parent = Inst (Arithmetic a)
createAux context (Terminal t) graph' =
  case t of
    MediumLevelILNoret _ -> graph'
    MediumLevelILBp _ -> graph'
    MediumLevelILJump MediumLevelILJumpRec {dest = d} -> graph'
    MediumLevelILGoto _ -> graph'
    MediumLevelILTrap _ -> graph'
    MediumLevelILJumpTo MediumLevelILJumpToRec {dest = d} -> graph'
    MediumLevelILIf MediumLevelILIfRec {condition = c} -> graph'
createAux context (Syscall s) graph' =
  case s of
    MediumLevelILSyscallUntyped MediumLevelILSyscallUntypedRec {params = p} -> graph'
    MediumLevelILSyscallSsa MediumLevelILSyscallSsaRec {params = p} -> graph'
    MediumLevelILSyscall MediumLevelILSyscallRec {params = p} -> graph'
    MediumLevelILSyscallUntypedSsa MediumLevelILSyscallUntypedSsaRec {params = p} -> graph'
createAux context (Tailcall t) graph' =
  case t of
    MediumLevelILTailcallUntyped MediumLevelILTailcallUntypedRec {dest = d, params = p} -> graph'
    MediumLevelILTailcall MediumLevelILTailcallRec {dest = d, params = p} -> graph'
    MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec {dest = d, params = p} -> graph'
    MediumLevelILTailcallUntypedSsa MediumLevelILTailcallUntypedSsaRec {dest = d, params = p} -> graph'
createAux context (ControlFlow (MediumLevelILRetHint MediumLevelILRetHintRec {dest = d})) graph' = graph'
createAux context (Return (MediumLevelILRet MediumLevelILRetRec {src = s})) graph' = graph'
createAux context (Load l) graph' =
  case l of
    MediumLevelILLoad MediumLevelILLoadRec {src = s} -> graph'
    MediumLevelILLoadStruct MediumLevelILLoadStructRec {src = s} -> graph'
    MediumLevelILLoadSsa MediumLevelILLoadSsaRec {src = s} -> graph'
    MediumLevelILLoadStructSsa MediumLevelILLoadStructSsaRec {src = s} -> graph'
createAux context (Store store') graph' =
  case store' of
    MediumLevelILStore MediumLevelILStoreRec {src = s, dest = d} -> graph'
    MediumLevelILStoreStruct MediumLevelILStoreStructRec {src = s, dest = d} -> graph'
    MediumLevelILStoreSsa MediumLevelILStoreSsaRec {src = s, dest = d} -> graph'
    MediumLevelILStoreStructSsa MediumLevelILStoreStructSsaRec {src = s, dest = d} -> graph'
    MediumLevelILStoreOutput MediumLevelILStoreOutputRec {dest = d} -> graph'
createAux context (Memory m) graph' =
  case m of
    MediumLevelILUnimplMem MediumLevelILUnimplMemRec {src = s} -> graph'
    MediumLevelILMemPhi _ -> graph'
createAux context (Carry carry') graph' =
  case carry' of
    MediumLevelILAdc MediumLevelILAdcRec {left = l, right = r, carry = c} -> graph'
    MediumLevelILSbb MediumLevelILSbbRec {left = l, right = r, carry = c} -> graph'
    MediumLevelILRlc MediumLevelILRlcRec {left = l, right = r, carry = c} -> graph'
    MediumLevelILRrc MediumLevelILRrcRec {left = l, right = r, carry = c} -> graph'
createAux context (SetVar sv) graph' =
  case sv of
    MediumLevelILSetVar MediumLevelILSetVarRec {src = s} -> graph'
    MediumLevelILVarPhi _ -> graph'
    MediumLevelILSetVarSsa MediumLevelILSetVarSsaRec {src = s} -> graph'
    MediumLevelILSetVarAliased MediumLevelILSetVarAliasedRec {src = s} -> graph'
    MediumLevelILSetVarSsaField MediumLevelILSetVarSsaFieldRec {src = s} -> graph'
    MediumLevelILSetVarSplitSsa MediumLevelILSetVarSplitSsaRec {src = s} -> graph'
    MediumLevelILSetVarAliasedField MediumLevelILSetVarAliasedFieldRec {src = s} -> graph'
    MediumLevelILSetVarField MediumLevelILSetVarFieldRec {src = s} -> graph'
    MediumLevelILSetVarSplit MediumLevelILSetVarSplitRec {src = s} -> graph'
    MediumLevelILVarOutputField MediumLevelILVarOutputFieldRec {dest = d, offset = o} -> graph'
    MediumLevelILVarOutputSsaField MediumLevelILVarOutputSsaFieldRec {dest = d, prev = p, offset = o} -> graph'
    MediumLevelILVarOutputAliased MediumLevelILVarOutputAliasedRec {dest = d, prev = p} -> graph'
    MediumLevelILVarOutputAliasedField MediumLevelILVarOutputAliasedFieldRec {dest = d, prev = p, offset = o} -> graph'
createAux context (RegisterStack _) graph' = graph'
createAux context (VariableInstruction _) graph' = graph'
createAux context (IntrinsicInstruction ii) graph' =
  case ii of
    MediumLevelILIntrinsic MediumLevelILIntrinsicRec {params = p} -> graph'
    MediumLevelILIntrinsicSsa MediumLevelILIntrinsicSsaRec {params = p} -> graph'
    MediumLevelILMemoryIntrinsicSsa MediumLevelILMemoryIntrinsicSsaRec {params = p} -> graph'
createAux context (MediumLevelILCallOutputSsa _) graph' = graph'
createAux context (MediumLevelILMemoryIntrinsicOutputSsa _) graph' = graph'
createAux context (MediumLevelILCallParamSsa MediumLevelILCallParamSsaRec {src = s}) graph' = graph'
createAux context (MediumLevelILCallParam MediumLevelILCallParamRec {src = s}) graph' = graph'
createAux context (MediumLevelILNop _) graph' = graph'
createAux context (MediumLevelILAddressOf _) graph' = graph'
createAux context (MediumLevelILAddressOfField _) graph' = graph'
createAux context (MediumLevelILPassByRef _) graph' = graph'
createAux context (MediumLevelILReturnByRef _) graph' = graph'
createAux context (MediumLevelILVarOutputSsa _) graph' = graph'
createAux context (MediumLevelILBlockToExpand _) graph' = graph'
createAux context (MediumLevelILMuluDp MediumLevelILMuluDpRec {left = l, right = r}) graph' = graph'
createAux context (MediumLevelILMulsDp MediumLevelILMulsDpRec {left = l, right = r}) graph' = graph'
createAux context (MediumLevelILDivuDp MediumLevelILDivuDpRec {left = l, right = r}) graph' = graph'
createAux context (MediumLevelILDivsDp MediumLevelILDivsDpRec {left = l, right = r}) graph' = graph'
createAux context (MediumLevelILModuDp MediumLevelILModuDpRec {left = l, right = r}) graph' = graph'
createAux context (MediumLevelILModsDp MediumLevelILModsDpRec {left = l, right = r}) graph' = graph'
createAux context (MediumLevelILBoolToInt MediumLevelILBoolToIntRec {src = s}) graph' = graph'
createAux context (MediumLevelILAssert _) graph' = graph'
createAux context (MediumLevelILAssertSsa _) graph' = graph'
createAux context (MediumLevelILForceVer _) graph' = graph'
createAux context (MediumLevelILForceVerSsa _) graph' = graph'
createAux context (MediumLevelILVarField _) graph' = graph'
createAux context (MediumLevelILVarSplit _) graph' = graph'
createAux context (MediumLevelILUndef _) graph' = graph'
createAux context (MediumLevelILUnimpl _) graph' = graph'
createAux context (MediumLevelILSeparateParamList MediumLevelILSeparateParamListRec {params = p}) graph' = graph'
createAux context (MediumLevelILSharedParamSlot MediumLevelILSharedParamSlotRec {params = p}) graph' = graph'
