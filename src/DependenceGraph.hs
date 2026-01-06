{-# LANGUAGE DuplicateRecordFields #-}

module DependenceGraph
  ( DependenceGraph.create
  , DependenceGraph.Graph
  )
where

import Binja.Types
import qualified Data.Set as Set
import qualified Data.Map as Map

type Vertex = MediumLevelILSSAInstruction
data Graph = Graph
  { root :: MediumLevelILSSAInstruction,
    graph :: Map.Map Vertex (Set.Set Vertex)
  }
  deriving (Show)

create :: MediumLevelILSSAInstruction -> Graph
create inst =
  Graph
  { root = inst,
    graph = createAux inst Map.empty
  }

createAux :: MediumLevelILSSAInstruction ->
             Map.Map Vertex (Set.Set Vertex) ->
             Map.Map Vertex (Set.Set Vertex)
createAux inst graph' =
  if Map.member inst graph'
    then graph'
    else foldl' (\acc i -> createAux i acc) updateGraph children'
  where
    children' = directDep inst
    updateGraph = Map.insert inst (Set.fromList children') graph'

-- Create a list of direct instructions which influence the value of the
-- given instruction.
-- TODO: (1) If a call instruction has a resolvable function then scan for
--           global variables/data that could influence the call instructions
--           result.
--       (2) Include defSite of SSAVariable in VariableInstruction case.
directDep :: MediumLevelILSSAInstruction -> [MediumLevelILSSAInstruction]
directDep (Localcall lc) =
  case lc of
    MediumLevelILCall MediumLevelILCallRec {dest = d, params = p} -> p ++ [d]
    MediumLevelILCallSsa MediumLevelILCallSsaRec {dest = d, params = p} -> p ++ [d]
    MediumLevelILCallUntyped MediumLevelILCallUntypedRec {dest = d, params = p} -> p ++ [d]
    MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec {dest = d, params = p} -> p ++ [d]
directDep (Constant _) = []
directDep (Comparison cmp) =
  case cmp of
    MediumLevelILCmpE MediumLevelILCmpERec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpE MediumLevelILFcmpERec {left = l, right = r} -> [l, r]
    MediumLevelILCmpNe MediumLevelILCmpNeRec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpNe MediumLevelILFcmpNeRec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpLt MediumLevelILFcmpLtRec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpLe MediumLevelILFcmpLeRec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpGe MediumLevelILFcmpGeRec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpGt MediumLevelILFcmpGtRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpSlt MediumLevelILCmpSltRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpUlt MediumLevelILCmpUltRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpSle MediumLevelILCmpSleRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpUle MediumLevelILCmpUleRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpSge MediumLevelILCmpSgeRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpUge MediumLevelILCmpUgeRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpSgt MediumLevelILCmpSgtRec {left = l, right = r} -> [l, r]
    MediumLevelILCmpUgt MediumLevelILCmpUgtRec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpO MediumLevelILFcmpORec {left = l, right = r} -> [l, r]
    MediumLevelILFcmpUo MediumLevelILFcmpUoRec {left = l, right = r} -> [l, r]
    MediumLevelILTestBit MediumLevelILTestBitRec {left = l, right = r} -> [l, r]
directDep (Arithmetic a) =
  case a of
    MediumLevelILNeg MediumLevelILNegRec {src = s} -> [s]
    MediumLevelILNot MediumLevelILNotRec {src = s} -> [s]
    MediumLevelILSx MediumLevelILSxRec {src = s} -> [s]
    MediumLevelILZx MediumLevelILZxRec {src = s} -> [s]
    MediumLevelILLowPart MediumLevelILLowPartRec {src = s} -> [s]
    MediumLevelILFsqrt MediumLevelILFsqrtRec {src = s} -> [s]
    MediumLevelILFneg MediumLevelILFnegRec {src = s} -> [s]
    MediumLevelILFabs MediumLevelILFabsRec {src = s} -> [s]
    MediumLevelILFloatToInt MediumLevelILFloatToIntRec {src = s} -> [s]
    MediumLevelILIntToFloat MediumLevelILIntToFloatRec {src = s} -> [s]
    MediumLevelILFloatConv MediumLevelILFloatConvRec {src = s} -> [s]
    MediumLevelILRoundToInt MediumLevelILRoundToIntRec {src = s} -> [s]
    MediumLevelILFloor MediumLevelILFloorRec {src = s} -> [s]
    MediumLevelILCeil MediumLevelILCeilRec {src = s} -> [s]
    MediumLevelILFtrunc MediumLevelILFtruncRec {src = s} -> [s]
    MediumLevelILAdd MediumLevelILAddRec {left = l, right = r} -> [l,r]
    MediumLevelILSub MediumLevelILSubRec {left = l, right = r} -> [l,r]
    MediumLevelILAnd MediumLevelILAndRec {left = l, right = r} -> [l,r]
    MediumLevelILOr MediumLevelILOrRec {left = l, right = r} -> [l,r]
    MediumLevelILXor MediumLevelILXorRec {left = l, right = r} -> [l,r]
    MediumLevelILLsl MediumLevelILLslRec {left = l, right = r} -> [l,r]
    MediumLevelILLsr MediumLevelILLsrRec {left = l, right = r} -> [l,r]
    MediumLevelILAsr MediumLevelILAsrRec {left = l, right = r} -> [l,r]
    MediumLevelILRol MediumLevelILRolRec {left = l, right = r} -> [l,r]
    MediumLevelILRor MediumLevelILRorRec {left = l, right = r} -> [l,r]
    MediumLevelILMul MediumLevelILMulRec {left = l, right = r} -> [l,r]
    MediumLevelILDivu MediumLevelILDivuRec {left = l, right = r} -> [l,r]
    MediumLevelILDivs MediumLevelILDivsRec {left = l, right = r} -> [l,r]
    MediumLevelILModu MediumLevelILModuRec {left = l, right = r} -> [l,r]
    MediumLevelILMods MediumLevelILModsRec {left = l, right = r} -> [l,r]
    MediumLevelILAddOverflow MediumLevelILAddOverflowRec {left = l, right = r} -> [l,r]
    MediumLevelILFadd MediumLevelILFaddRec {left = l, right = r} -> [l,r]
    MediumLevelILFsub MediumLevelILFsubRec {left = l, right = r} -> [l,r]
    MediumLevelILFmul MediumLevelILFmulRec {left = l, right = r} -> [l,r]
    MediumLevelILFdiv MediumLevelILFdivRec {left = l, right = r} -> [l,r]
directDep (Terminal t) =
  case t of
    MediumLevelILNoret _ -> []
    MediumLevelILBp _ -> []
    MediumLevelILJump MediumLevelILJumpRec {dest = d} -> [d]
    MediumLevelILGoto _ -> []
    MediumLevelILTrap _ -> []
    MediumLevelILJumpTo MediumLevelILJumpToRec {dest = d} -> [d]
    MediumLevelILIf MediumLevelILIfRec {condition = c} -> [c]
directDep (Syscall s) =
  case s of
    MediumLevelILSyscallUntyped MediumLevelILSyscallUntypedRec {params = p} -> p
    MediumLevelILSyscallSsa MediumLevelILSyscallSsaRec {params = p} -> p
    MediumLevelILSyscall MediumLevelILSyscallRec {params = p} -> p
    MediumLevelILSyscallUntypedSsa MediumLevelILSyscallUntypedSsaRec {params = p} -> p
directDep (Tailcall t) =
  case t of
    MediumLevelILTailcallUntyped MediumLevelILTailcallUntypedRec {dest = d, params = p} ->
      p ++ [d]
    MediumLevelILTailcall MediumLevelILTailcallRec {dest = d, params = p} ->
      p ++ [d]
    MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec {dest = d, params = p} ->
      p ++ [d]
    MediumLevelILTailcallUntypedSsa MediumLevelILTailcallUntypedSsaRec {dest = d, params = p} ->
      p ++ [d]
directDep (ControlFlow (MediumLevelILRetHint MediumLevelILRetHintRec {dest = d})) = [d]
directDep (Return (MediumLevelILRet MediumLevelILRetRec {src = s})) = s
directDep (Load l) =
  case l of
    MediumLevelILLoad MediumLevelILLoadRec {src = s} -> [s]
    MediumLevelILLoadStruct MediumLevelILLoadStructRec {src = s} -> [s]
    MediumLevelILLoadSsa MediumLevelILLoadSsaRec {src = s} -> [s]
    MediumLevelILLoadStructSsa MediumLevelILLoadStructSsaRec {src = s} -> [s]
directDep (Store store') =
  case store' of
    MediumLevelILStore MediumLevelILStoreRec {src = s, dest = d} -> [s,d]
    MediumLevelILStoreStruct MediumLevelILStoreStructRec {src = s, dest = d} -> [s,d]
    MediumLevelILStoreSsa MediumLevelILStoreSsaRec {src = s, dest = d} -> [s,d]
    MediumLevelILStoreStructSsa MediumLevelILStoreStructSsaRec {src = s, dest = d} -> [s,d]
directDep (Memory m) =
  case m of
    MediumLevelILUnimplMem MediumLevelILUnimplMemRec {src = s} -> [s]
    MediumLevelILMemPhi _ -> []
directDep (Carry carry') =
  case carry' of
    MediumLevelILAdc MediumLevelILAdcRec {left = l, right = r, carry = c} -> [l,r,c]
    MediumLevelILSbb MediumLevelILSbbRec {left = l, right = r, carry = c} -> [l,r,c]
    MediumLevelILRlc MediumLevelILRlcRec {left = l, right = r, carry = c} -> [l,r,c]
    MediumLevelILRrc MediumLevelILRrcRec {left = l, right = r, carry = c} -> [l,r,c]
directDep (SetVar sv) =
  case sv of
    MediumLevelILSetVar MediumLevelILSetVarRec {src = s} -> [s]
    MediumLevelILVarPhi _ -> []
    MediumLevelILSetVarSsa MediumLevelILSetVarSsaRec {src = s} -> [s]
    MediumLevelILSetVarAliased MediumLevelILSetVarAliasedRec {src = s} -> [s]
    MediumLevelILSetVarSsaField MediumLevelILSetVarSsaFieldRec {src = s} -> [s]
    MediumLevelILSetVarSplitSsa MediumLevelILSetVarSplitSsaRec {src = s} -> [s]
    MediumLevelILSetVarAliasedField MediumLevelILSetVarAliasedFieldRec {src = s} -> [s]
    MediumLevelILSetVarField MediumLevelILSetVarFieldRec {src = s} -> [s]
    MediumLevelILSetVarSplit MediumLevelILSetVarSplitRec {src = s} -> [s]
directDep (RegisterStack _) = []
directDep (VariableInstruction var) = []
  --case var of
    --MediumLevelILVarSsa MediumLevelILVarSsaRec {src = s} -> 
    --MediumLevelILVarAliased MediumLevelILVarAliasedRec {src = s} -> 
    --MediumLevelILVarSsaField MediumLevelILVarSsaFieldRec {src = s} -> 
    --MediumLevelILVarAliasedField MediumLevelILVarAliasedFieldRec {src = s} -> 
    --MediumLevelILVarSplitSsa MediumLevelILVarSplitSsaRec {src = s} -> 

directDep (IntrinsicInstruction ii) =
  case ii of
    MediumLevelILIntrinsic MediumLevelILIntrinsicRec {params = p} -> p
    MediumLevelILIntrinsicSsa MediumLevelILIntrinsicSsaRec {params = p} -> p
    MediumLevelILMemoryIntrinsicSsa MediumLevelILMemoryIntrinsicSsaRec {params = p} -> p
directDep (MediumLevelILCallOutputSsa _) = []
directDep (MediumLevelILCallOutput _) = []
directDep (MediumLevelILMemoryIntrinsicOutputSsa _) = []
directDep (MediumLevelILCallParamSsa MediumLevelILCallParamSsaRec {src = s}) = s
directDep (MediumLevelILCallParam MediumLevelILCallParamRec {src = s}) = s
directDep (MediumLevelILNop _) = []
directDep (MediumLevelILAddressOf _) = []
directDep (MediumLevelILAddressOfField _) = []
directDep (MediumLevelILMuluDp MediumLevelILMuluDpRec {left = l, right = r}) = [l,r]
directDep (MediumLevelILMulsDp MediumLevelILMulsDpRec {left = l, right = r}) = [l,r]
directDep (MediumLevelILDivuDp MediumLevelILDivuDpRec {left = l, right = r}) = [l,r]
directDep (MediumLevelILDivsDp MediumLevelILDivsDpRec {left = l, right = r}) = [l,r]
directDep (MediumLevelILModuDp MediumLevelILModuDpRec {left = l, right = r}) = [l,r]
directDep (MediumLevelILModsDp MediumLevelILModsDpRec {left = l, right = r}) = [l,r]
directDep (MediumLevelILBoolToInt MediumLevelILBoolToIntRec {src = s}) = [s]
directDep (MediumLevelILAssert _) = []
directDep (MediumLevelILAssertSsa _) = []
directDep (MediumLevelILForceVer _) = []
directDep (MediumLevelILForceVerSsa _) = []
directDep (MediumLevelILVarField _) = []
directDep (MediumLevelILVarSplit _) = []
directDep (MediumLevelILUndef _) = []
directDep (MediumLevelILUnimpl _) = []
directDep (MediumLevelILSeparateParamList MediumLevelILSeparateParamListRec {params = p}) = p
directDep (MediumLevelILSharedParamSlot MediumLevelILSharedParamSlotRec {params = p}) = p
