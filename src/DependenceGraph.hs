{-# LANGUAGE DuplicateRecordFields #-}

module DependenceGraph
  ( DependenceGraph.create,
  )
where

import Binja.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

type Vertex = Binja.Types.MediumLevelILSSAInstruction

type Graph = Map.Map Vertex (Set.Set Vertex)

create :: AnalysisContext -> MediumLevelILSSAInstruction -> Graph
create context inst = createAux context inst Map.empty

createAux :: AnalysisContext -> MediumLevelILSSAInstruction -> Graph -> Graph
createAux context (Localcall lc) graph' =
  case lc of
    MediumLevelILCall MediumLevelILCallRec {dest = d, params = p} -> graph'
    MediumLevelILCallSsa MediumLevelILCallSsaRec {dest = d, params = p} -> graph'
    MediumLevelILCallUntyped MediumLevelILCallUntypedRec {dest = d, params = p} -> graph'
    MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec {dest = d, params = p} -> graph'
createAux context (Constant _) graph' = graph'
createAux context (Comparison cmp) graph' =
  case cmp of
    MediumLevelILCmpE MediumLevelILCmpERec {left = l, right = r} -> graph'
    MediumLevelILFcmpE MediumLevelILFcmpERec {left = l, right = r} -> graph'
    MediumLevelILCmpNe MediumLevelILCmpNeRec {left = l, right = r} -> graph'
    MediumLevelILFcmpNe MediumLevelILFcmpNeRec {left = l, right = r} -> graph'
    MediumLevelILFcmpLt MediumLevelILFcmpLtRec {left = l, right = r} -> graph'
    MediumLevelILFcmpLe MediumLevelILFcmpLeRec {left = l, right = r} -> graph'
    MediumLevelILFcmpGe MediumLevelILFcmpGeRec {left = l, right = r} -> graph'
    MediumLevelILFcmpGt MediumLevelILFcmpGtRec {left = l, right = r} -> graph'
    MediumLevelILCmpSlt MediumLevelILCmpSltRec {left = l, right = r} -> graph'
    MediumLevelILCmpUlt MediumLevelILCmpUltRec {left = l, right = r} -> graph'
    MediumLevelILCmpSle MediumLevelILCmpSleRec {left = l, right = r} -> graph'
    MediumLevelILCmpUle MediumLevelILCmpUleRec {left = l, right = r} -> graph'
    MediumLevelILCmpSge MediumLevelILCmpSgeRec {left = l, right = r} -> graph'
    MediumLevelILCmpUge MediumLevelILCmpUgeRec {left = l, right = r} -> graph'
    MediumLevelILCmpSgt MediumLevelILCmpSgtRec {left = l, right = r} -> graph'
    MediumLevelILCmpUgt MediumLevelILCmpUgtRec {left = l, right = r} -> graph'
    MediumLevelILFcmpO MediumLevelILFcmpORec {left = l, right = r} -> graph'
    MediumLevelILFcmpUo MediumLevelILFcmpUoRec {left = l, right = r} -> graph'
    MediumLevelILTestBit MediumLevelILTestBitRec {left = l, right = r} -> graph'
createAux context (Arithmetic a) graph' =
  case a of
    MediumLevelILNeg MediumLevelILNegRec {src = s} -> graph' 
    MediumLevelILNot MediumLevelILNotRec {src = s} -> graph'
    MediumLevelILSx MediumLevelILSxRec {src = s} -> graph' 
    MediumLevelILZx MediumLevelILZxRec {src = s} -> graph' 
    MediumLevelILLowPart MediumLevelILLowPartRec {src = s} -> graph' 
    MediumLevelILFsqrt MediumLevelILFsqrtRec {src = s} -> graph' 
    MediumLevelILFneg MediumLevelILFnegRec {src = s} -> graph' 
    MediumLevelILFabs MediumLevelILFabsRec {src = s} -> graph' 
    MediumLevelILFloatToInt MediumLevelILFloatToIntRec {src = s} -> graph' 
    MediumLevelILIntToFloat MediumLevelILIntToFloatRec {src = s} -> graph' 
    MediumLevelILFloatConv MediumLevelILFloatConvRec {src = s} -> graph' 
    MediumLevelILRoundToInt MediumLevelILRoundToIntRec {src = s} -> graph' 
    MediumLevelILFloor MediumLevelILFloorRec {src = s} -> graph' 
    MediumLevelILCeil MediumLevelILCeilRec {src = s} -> graph' 
    MediumLevelILFtrunc MediumLevelILFtruncRec {src = s} -> graph' 
    MediumLevelILAdd MediumLevelILAddRec {left = l, right = r} -> graph'
    MediumLevelILSub MediumLevelILSubRec {left = l, right = r} -> graph'
    MediumLevelILAnd MediumLevelILAndRec {left = l, right = r} -> graph'
    MediumLevelILOr MediumLevelILOrRec {left = l, right = r} -> graph'
    MediumLevelILXor MediumLevelILXorRec {left = l, right = r} -> graph'
    MediumLevelILLsl MediumLevelILLslRec {left = l, right = r} -> graph'
    MediumLevelILLsr MediumLevelILLsrRec {left = l, right = r} -> graph'
    MediumLevelILAsr MediumLevelILAsrRec {left = l, right = r} -> graph'
    MediumLevelILRol MediumLevelILRolRec {left = l, right = r} -> graph'
    MediumLevelILRor MediumLevelILRorRec {left = l, right = r} -> graph'
    MediumLevelILMul MediumLevelILMulRec {left = l, right = r} -> graph'
    MediumLevelILDivu MediumLevelILDivuRec {left = l, right = r} -> graph'
    MediumLevelILDivs MediumLevelILDivsRec {left = l, right = r} -> graph'
    MediumLevelILModu MediumLevelILModuRec {left = l, right = r} -> graph'
    MediumLevelILMods MediumLevelILModsRec {left = l, right = r} -> graph'
    MediumLevelILAddOverflow MediumLevelILAddOverflowRec {left = l, right = r} -> graph'
    MediumLevelILFadd MediumLevelILFaddRec {left = l, right = r} -> graph'
    MediumLevelILFsub MediumLevelILFsubRec {left = l, right = r} -> graph'
    MediumLevelILFmul MediumLevelILFmulRec {left = l, right = r} -> graph'
    MediumLevelILFdiv MediumLevelILFdivRec {left = l, right = r} -> graph'
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
createAux context (RegisterStack _) graph' = graph'
createAux context (VariableInstruction _) graph' = graph'
createAux context (IntrinsicInstruction ii) graph' =
  case ii of
    MediumLevelILIntrinsic MediumLevelILIntrinsicRec {params = p} -> graph'
    MediumLevelILIntrinsicSsa MediumLevelILIntrinsicSsaRec {params = p} -> graph'
    MediumLevelILMemoryIntrinsicSsa MediumLevelILMemoryIntrinsicSsaRec {params = p} -> graph'
createAux context (MediumLevelILCallOutputSsa _) graph' = graph'
createAux context (MediumLevelILCallOutput _) graph' = graph'
createAux context (MediumLevelILMemoryIntrinsicOutputSsa _) graph' = graph'
createAux context (MediumLevelILCallParamSsa MediumLevelILCallParamSsaRec {src = s}) graph' = graph'
createAux context (MediumLevelILCallParam MediumLevelILCallParamRec {src = s}) graph' = graph'
createAux context (MediumLevelILNop _) graph' = graph'
createAux context (MediumLevelILAddressOf _) graph' = graph'
createAux context (MediumLevelILAddressOfField _) graph' = graph'
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
