{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Binja.Mlil
-- Description : Medium Level IL instruction interface
-- License     : MIT
-- Maintainer  : hello@bloombit.dev
-- Stability   : alpha
--
-- @Binja.Mlil@ provides higher level types for medium level IL SSA-variant instructions and utility functions.
--
-- Official Mlil Documentation: <https://docs.binary.ninja/dev/bnil-mlil.html Binary Ninja Intermediate Language: Medium Level IL>
module Binja.Mlil
  ( Binja.Mlil.fromRef,
    Binja.Mlil.callerSites,
    Binja.Mlil.defSite,
    Binja.Mlil.useSites,
    Binja.Mlil.constantToSymbol,
    Binja.Mlil.extractCallDestSymbol,
    Binja.Mlil.instructions,
    Binja.Mlil.instructionsFromFunc,
    Binja.Mlil.instructionsFromFuncNoChildren,
    Binja.Mlil.children,
  )
where

import Binja.BinaryView
import Binja.FFI
import Binja.Function
import Binja.Llil
import Binja.ReferenceSource
import Binja.Types
import Control.Monad (zipWithM)
import Data.Maybe (catMaybes)

startIndex :: BNMlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize
startIndex func arch' addr = do
  if arch' == nullPtr || func == nullPtr
    then error "Binja.Mlil.startIndex: called with nullPtr argument"
    else do
      startI <- c_BNMediumLevelILGetInstructionStart func arch' addr
      count' <- c_BNGetMediumLevelILInstructionCount func
      -- Ensure start index is less than total mlil instructions
      -- in function
      if startI >= count'
        then error $ "Binja.Mlil.startIndex: startI:" ++ show startI ++ " >= count:" ++ show count'
        else pure startI

-- | Construct a raw mlil instruction from a ssa function and expression index
mlilSSAByIndex :: BNMlilSSAFunctionPtr -> CSize -> IO BNMediumLevelILInstruction
mlilSSAByIndex func index' = do
  alloca $ \p -> do
    _ <- c_BNGetMediumLevelILByIndexPtr p func index'
    peek p

-- | Retrieve the best MLIL SSA-variant instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO MediumLevelILSSAInstruction
fromRef ref = do
  -- Get mlil (non-ssa) expression index
  func <- Binja.Function.mlil (bnFunc ref)
  sIndex <- Binja.Mlil.startIndex func (bnArch ref) (bnAddr ref)
  exprIndex' <- c_BNGetMediumLevelILIndexForInstruction func (fromIntegral sIndex)
  -- Convert func and expression index to SSA
  funcSSA <- Binja.Function.mlilToSSA func
  ssaExprIndex <- c_BNGetMediumLevelILSSAExprIndex func exprIndex'
  create funcSSA ssaExprIndex

fromLlilRef :: BNReferenceSource -> IO MediumLevelILSSAInstruction
fromLlilRef ref = do
  ssaExprIndex <- Binja.Llil.llilRefToMlilExprIndex ref
  func <- Binja.Function.mlilSSA (bnFunc ref)
  create func ssaExprIndex

getExprList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [MediumLevelILSSAInstruction]
getExprList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count' <- fromIntegral <$> peek countPtr
    xs <-
      if rawPtr == nullPtr || count' == 0
        then pure []
        else peekArray count' rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    mapM (create func . fromIntegral) xs

getExpr :: BNMlilSSAFunctionPtr -> CSize -> IO MediumLevelILSSAInstruction
getExpr = create

getInt :: BNMediumLevelILInstruction -> CSize -> IO Int
getInt inst operand = pure $ fromIntegral $ getOp inst operand

getIntList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [Int]
getIntList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count' <- fromIntegral <$> peek countPtr
    xs <-
      if rawPtr == nullPtr || count' == 0
        then pure []
        else peekArray count' rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    pure $ Prelude.map fromIntegral xs

varFromID :: CULLong -> IO BNVariable
varFromID index' =
  alloca $ \p -> do
    _ <- c_BNFromVariableIdentifierPtr p index'
    peek p

getVarList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [BNVariable]
getVarList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count' <- fromIntegral <$> peek countPtr
    xs <-
      if rawPtr == nullPtr || count' == 0
        then pure []
        else peekArray count' rawPtr
    when (rawPtr /= nullPtr) $ c_BNMediumLevelILFreeOperandList rawPtr
    mapM varFromID xs

getSSAVarList :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO [BNSSAVariable]
getSSAVarList func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count' <- fromIntegral <$> peek countPtr :: IO Int
    let pairCount = div count' 2
    result <-
      if rawPtr == nullPtr || pairCount == 0
        then pure []
        else forM [0 .. pairCount - 1] $ \j -> do
          varId <- peekElemOff rawPtr (j * 2)
          ver <- peekElemOff rawPtr (j * 2 + 1)
          v <- varFromID varId
          pure (BNSSAVariable v (fromIntegral ver))
    when (rawPtr /= nullPtr) $
      c_BNMediumLevelILFreeOperandList rawPtr
    pure result

getSSAVar :: BNMediumLevelILInstruction -> CSize -> CSize -> IO BNSSAVariable
getSSAVar inst varOP version' = do
  rawVar' <- varFromID $ fromIntegral $ getOp inst varOP
  pure $ BNSSAVariable rawVar' $ fromIntegral $ getOp inst version'

getSSAVarAndDest :: BNMediumLevelILInstruction -> CSize -> CSize -> IO BNSSAVariable
getSSAVarAndDest = getSSAVar

getFloat :: BNMediumLevelILInstruction -> CSize -> IO Double
getFloat inst index' =
  case mlSize inst of
    4 -> pure $ float2Double $ castWord32ToFloat w32
    8 -> pure $ castWord64ToDouble w64
    _ -> pure $ fromIntegral value
  where
    w64 = fromIntegral value :: Word64
    w32 = fromIntegral $ w64 .&. 0xffffffff :: Word32
    value = getOp inst index'

-- TODO: Lift BNDataBufferPtr into a higher type.
-- Currently this is uniquely used by MediumLevelILConstData
getConstantData :: BNFunctionPtr -> BNMediumLevelILInstruction -> CSize -> CSize -> IO BNDataBufferPtr
getConstantData func inst op1 op2 =
  c_BNGetConstantData func state value (mlSize inst) nullPtr
  where
    state = getOp inst op1
    value = getOp inst op2

getTargetMap :: BNMlilSSAFunctionPtr -> CSize -> CSize -> IO TargetMap
getTargetMap func expr operand =
  alloca $ \countPtr -> do
    rawPtr <- c_BNMediumLevelILGetOperandList func expr operand countPtr
    count' <- fromIntegral <$> peek countPtr :: IO Int
    let pairCount = div count' 2
    pairs <-
      if rawPtr == nullPtr || pairCount == 0
        then pure []
        else forM [0 .. pairCount - 1] $ \j -> do
          key <- peekElemOff rawPtr (j * 2)
          target' <- peekElemOff rawPtr (j * 2 + 1)
          pure (key, target')
    when (rawPtr /= nullPtr) $
      c_BNMediumLevelILFreeOperandList rawPtr
    pure pairs

getIntrinsicIL :: BNMediumLevelILInstruction -> BNMlilSSAFunctionPtr -> CSize -> IO ILIntrinsic
getIntrinsicIL inst func operand = do
  let index' = getOp inst operand
  rawFunc <- Binja.Function.mlilToRawFunction func
  archTy <- Binja.Function.architecture func
  archHandle' <- c_BNGetFunctionArchitecture rawFunc
  intrinsic' <- getIntrinsic archTy index'
  pure $ ILIntrinsic index' archHandle' archTy intrinsic'

getConstraint :: BNMlilSSAFunctionPtr -> BNMediumLevelILInstruction -> CSize -> IO BNPossibleValueSet
getConstraint func inst operand = do
  alloca $ \p -> do
    _ <- c_BNGetCachedMediumLevelILPossibleValueSetPtr p func constraintIndex
    peek p
  where
    constraintIndex = getOp inst operand

-- | All top-level instructions in a specific function (children not included).
instructionsFromFuncNoChildren :: BNMlilSSAFunctionPtr -> IO [MediumLevelILSSAInstruction]
instructionsFromFuncNoChildren func = do
  count' <- fromIntegral <$> c_BNGetMediumLevelILSSAInstructionCount func
  exprs <- mapM (c_BNGetMediumLevelILSSAIndexForInstruction func) [0 .. count' - 1]
  mapM (create func) exprs

-- | All instructions (children included) in a specific function.
instructionsFromFunc :: BNMlilSSAFunctionPtr -> IO [MediumLevelILSSAInstruction]
instructionsFromFunc func = do
  count' <- c_BNGetMediumLevelILSSAExprCount func
  mapM (create func) [0 .. count' - 1]

-- | All instructions (children included) in a binary view.
instructions :: BNBinaryViewPtr -> IO [MediumLevelILSSAInstruction]
instructions view = do
  rawFuncs <- Binja.BinaryView.functions view
  mlilSSAFuncs <- mapM Binja.Function.mlilSSA rawFuncs
  insts <- mapM instructionsFromFunc mlilSSAFuncs
  pure $ concat insts

callerSites :: BNBinaryViewPtr -> BNMlilSSAFunctionPtr -> IO [MediumLevelILSSAInstruction]
callerSites view func = do
  rawFunc <- mlilToRawFunction func
  start' <- Binja.Function.start rawFunc
  -- These reference sources are llil.
  -- lift via fromLlilRef
  refs' <- Binja.ReferenceSource.codeRefs view start'
  insts' <- mapM Binja.Mlil.fromLlilRef refs'
  pure $ Prelude.filter isLocalcall insts'
  where
    isLocalcall :: MediumLevelILSSAInstruction -> Bool
    isLocalcall (Localcall _) = True
    isLocalcall _ = False

-- | Derive a definition site from a ssa variable in a medium level IL SSA-variant function if
--   exists. Function arguments and registers of version 0 do not have definition sites for example.
defSite :: BNSSAVariable -> BNMlilSSAFunctionPtr -> IO (Maybe MediumLevelILSSAInstruction)
defSite ssaVar funcSSA =
  alloca $ \varPtr' -> do
    poke varPtr' $ rawVar ssaVar
    instrSSAIndex <-
      c_BNGetMediumLevelILSSAVarDefinition
        funcSSA
        varPtr'
        (fromIntegral $ version ssaVar)
    exprIndexSSA <- c_BNGetMediumLevelILSSAIndexForInstruction funcSSA $ fromIntegral instrSSAIndex
    instCount <- c_BNGetMediumLevelILSSAInstructionCount funcSSA
    if instrSSAIndex >= instCount
      then pure Nothing
      else Just <$> create funcSSA exprIndexSSA

useSites :: BNSSAVariable -> BNMlilSSAFunctionPtr -> IO [MediumLevelILSSAInstruction]
useSites ssaVar funcSSA =
  alloca $ \countPtr -> do
    alloca $ \varPtr' -> do
      poke varPtr' $ rawVar ssaVar
      rawResult <-
        c_BNGetMediumLevelILSSAVarUses
          funcSSA
          varPtr'
          (fromIntegral $ version ssaVar)
          countPtr
      count' <- fromIntegral <$> peek countPtr :: IO Int
      result <-
        if rawResult == nullPtr || count' == 0
          then pure []
          else peekArray count' rawResult
      exprIndexSSAList <- mapM (c_BNGetMediumLevelILSSAIndexForInstruction funcSSA . fromIntegral) result
      instCount <- c_BNGetMediumLevelILSSAInstructionCount funcSSA
      useSites' <-
        catMaybes
          <$> zipWithM
            ( \instrIndex exprIndex' ->
                createMaybe instCount instrIndex exprIndex' funcSSA
            )
            result
            exprIndexSSAList
      when (rawResult /= nullPtr) $ c_BNFreeILInstructionList rawResult
      pure useSites'
  where
    createMaybe :: CSize -> CSize -> CSize -> BNMlilSSAFunctionPtr -> IO (Maybe MediumLevelILSSAInstruction)
    createMaybe instCount instrIndex exprIndex' funcSSA' =
      if instrIndex >= instCount
        then pure Nothing
        else Just <$> create funcSSA' exprIndex'

-- Convert Constant instruction to symbol if possible
constantToSymbol :: BNBinaryViewPtr -> Constant -> IO (Maybe Symbol)
constantToSymbol view' (MediumLevelILConstPtr (MediumLevelILConstPtrRec {constant = c})) = do
  Binja.BinaryView.symbolAt view' $ fromIntegral c
constantToSymbol view' (MediumLevelILImport (MediumLevelILImportRec {constant = c})) = do
  Binja.BinaryView.symbolAt view' $ fromIntegral c
constantToSymbol _ (MediumLevelILConst (MediumLevelILConstRec {constant = c})) = do
  Prelude.print $ "Unhandled constant: " ++ show c
  pure Nothing
constantToSymbol _ (MediumLevelILFloatConst MediumLevelILFloatConstRec {constant = c}) = do
  Prelude.print $ "Unhandled float constant: " ++ show c
  pure Nothing
constantToSymbol _ (MediumLevelILConstData MediumLevelILConstDataRec {constant = c}) = do
  Prelude.print $ "Unhandled constant data: " ++ show c
  pure Nothing
constantToSymbol view' (MediumLevelILExternPtr MediumLevelILExternPtrRec {constant = c}) = do
  Binja.BinaryView.symbolAt view' $ fromIntegral c

extractCallDestSymbol :: BNBinaryViewPtr -> MediumLevelILSSAInstruction -> IO (Maybe Symbol)
extractCallDestSymbol view callInst =
  case callInst of
    Localcall lc ->
      case lc of
        (MediumLevelILCall MediumLevelILCallRec {dest = d}) -> processDest d
        (MediumLevelILCallSsa MediumLevelILCallSsaRec {dest = d}) -> processDest d
        (MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec {dest = d}) -> processDest d
        (MediumLevelILCallUntyped MediumLevelILCallUntypedRec {dest = d}) -> processDest d
    Tailcall tc ->
      case tc of
        (MediumLevelILTailcallUntyped MediumLevelILTailcallUntypedRec {dest = d}) -> processDest d
        (MediumLevelILTailcall MediumLevelILTailcallRec {dest = d}) -> processDest d
        (MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec {dest = d}) -> processDest d
        (MediumLevelILTailcallUntypedSsa MediumLevelILTailcallUntypedSsaRec {dest = d}) -> processDest d
    _ -> error $ "Binja.Mlil.extractCallDestSymbol: unhandled instruction: " ++ show callInst
  where
    processDest :: MediumLevelILSSAInstruction -> IO (Maybe Symbol)
    processDest dest' =
      case dest' of
        Constant c -> constantToSymbol view c
        _ -> pure Nothing

getOp :: BNMediumLevelILInstruction -> CSize -> CSize
getOp inst operand =
  fromIntegral $ case operand of
    0 -> mlOp0 inst
    1 -> mlOp1 inst
    2 -> mlOp2 inst
    3 -> mlOp3 inst
    4 -> mlOp4 inst
    _ -> error $ "getOp: " ++ show operand ++ " not in [0, .., 4]"

-- | Derive a higher level mlil ssa-variant instruction given a mlil ssa function handle
--   and mlil ssa expression index.
create :: BNMlilSSAFunctionPtr -> CSize -> IO MediumLevelILSSAInstruction
create func exprIndex' = do
  rawInst <- mlilSSAByIndex func exprIndex'
  let coreInst =
        CoreMediumLevelILInstruction
          { instr = rawInst,
            ilFunc = func,
            exprIndex = exprIndex'
          }
  case mlOperation rawInst of
    MLIL_NOP -> do
      pure $ MediumLevelILNop $ MediumLevelILNopRec {core = coreInst}
    MLIL_SET_VAR -> do
      dest' <- varFromID $ fromIntegral $ getOp rawInst 0
      src' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILSetVarRec
              { dest = dest',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVar rec
    MLIL_SET_VAR_FIELD -> do
      dest' <- varFromID $ fromIntegral $ getOp rawInst 0
      offset' <- getInt rawInst 1
      src' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILSetVarFieldRec
              { dest = dest',
                offset = offset',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarField rec
    MLIL_SET_VAR_SPLIT -> do
      high' <- varFromID $ fromIntegral $ getOp rawInst 0
      low' <- varFromID $ fromIntegral $ getOp rawInst 1
      src' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILSetVarSplitRec
              { high = high',
                low = low',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarSplit rec
    MLIL_ASSERT -> do
      src' <- varFromID $ fromIntegral $ getOp rawInst 0
      constraint' <- getConstraint func rawInst 1
      let rec =
            MediumLevelILAssertRec
              { src = src',
                constraint = constraint',
                core = coreInst
              }
      pure $ MediumLevelILAssert rec
    MLIL_FORCE_VER -> do
      dest' <- varFromID $ fromIntegral $ getOp rawInst 0
      src' <- varFromID $ fromIntegral $ getOp rawInst 1
      let rec =
            MediumLevelILForceVerRec
              { dest = dest',
                src = src',
                core = coreInst
              }
      pure $ MediumLevelILForceVer rec
    MLIL_LOAD -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILLoadRec
              { src = src',
                core = coreInst
              }
      pure $ Load $ MediumLevelILLoad rec
    MLIL_LOAD_STRUCT -> do
      src' <- getExpr func $ getOp rawInst 0
      offset' <- getInt rawInst 1
      let rec =
            MediumLevelILLoadStructRec
              { src = src',
                offset = offset',
                core = coreInst
              }
      pure $ Load $ MediumLevelILLoadStruct rec
    MLIL_STORE -> do
      src' <- getExpr func $ getOp rawInst 0
      dest' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILStoreRec
              { src = src',
                dest = dest',
                core = coreInst
              }
      pure $ Store $ MediumLevelILStore rec
    MLIL_STORE_STRUCT -> do
      src' <- getExpr func $ getOp rawInst 0
      offset' <- getInt rawInst 1
      dest' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILStoreStructRec
              { src = src',
                offset = offset',
                dest = dest',
                core = coreInst
              }
      pure $ Store $ MediumLevelILStoreStruct rec
    MLIL_VAR -> do
      src' <- varFromID $ fromIntegral $ getOp rawInst 0
      let rec =
            MediumLevelILVarRec
              { src = src',
                var = src',
                core = coreInst
              }
      pure $ VariableInstruction $ MediumLevelILVar rec
    MLIL_VAR_FIELD -> do
      src' <- varFromID $ fromIntegral $ getOp rawInst 0
      offset' <- getInt rawInst 1
      let rec =
            MediumLevelILVarFieldRec
              { src = src',
                offset = offset',
                core = coreInst
              }
      pure $ MediumLevelILVarField rec
    MLIL_VAR_SPLIT -> do
      high' <- varFromID $ fromIntegral $ getOp rawInst 0
      low' <- varFromID $ fromIntegral $ getOp rawInst 1
      let rec =
            MediumLevelILVarSplitRec
              { high = high',
                low = low',
                core = coreInst
              }
      pure $ MediumLevelILVarSplit rec
    MLIL_ADDRESS_OF -> do
      src' <- varFromID $ fromIntegral $ getOp rawInst 0
      let rec =
            MediumLevelILAddressOfRec
              { src = src',
                core = coreInst
              }
      pure $ MediumLevelILAddressOf rec
    MLIL_ADDRESS_OF_FIELD -> do
      src' <- varFromID $ fromIntegral $ getOp rawInst 0
      offset' <- getInt rawInst 1
      let rec =
            MediumLevelILAddressOfFieldRec
              { src = src',
                offset = offset',
                core = coreInst
              }
      pure $ MediumLevelILAddressOfField rec
    MLIL_CONST -> do
      constant' <- getInt rawInst 0
      let rec =
            MediumLevelILConstRec
              { constant = constant',
                core = coreInst
              }
      pure $ Constant $ MediumLevelILConst rec
    MLIL_CONST_DATA -> do
      rawFunc <- mlilToRawFunction func
      constant' <- getConstantData rawFunc rawInst 0 1
      let rec =
            MediumLevelILConstDataRec
              { constant = constant',
                core = coreInst
              }
      pure $ Constant $ MediumLevelILConstData rec
    MLIL_CONST_PTR -> do
      constant' <- getInt rawInst 0
      let rec =
            MediumLevelILConstPtrRec
              { constant = constant',
                core = coreInst
              }
      pure $ Constant $ MediumLevelILConstPtr rec
    MLIL_EXTERN_PTR -> do
      constant' <- getInt rawInst 0
      offset' <- getInt rawInst 1
      let rec =
            MediumLevelILExternPtrRec
              { constant = constant',
                offset = offset',
                core = coreInst
              }
      pure $ Constant $ MediumLevelILExternPtr rec
    MLIL_FLOAT_CONST -> do
      constant' <- getFloat rawInst 0
      let rec =
            MediumLevelILFloatConstRec
              { constant = constant',
                core = coreInst
              }
      pure $ Constant $ MediumLevelILFloatConst rec
    MLIL_IMPORT -> do
      constant' <- getInt rawInst 0
      let rec =
            MediumLevelILImportRec
              { constant = constant',
                core = coreInst
              }
      pure $ Constant $ MediumLevelILImport rec
    MLIL_ADD -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILAddRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILAdd rec
    MLIL_ADC -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      carry' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILAdcRec
              { left = left',
                right = right',
                carry = carry',
                core = coreInst
              }
      pure $ Carry $ MediumLevelILAdc rec
    MLIL_SUB -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILSubRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILSub rec
    MLIL_SBB -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      carry' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILSbbRec
              { left = left',
                right = right',
                carry = carry',
                core = coreInst
              }
      pure $ Carry $ MediumLevelILSbb rec
    MLIL_AND -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILAndRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILAnd rec
    MLIL_OR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILOrRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILOr rec
    MLIL_XOR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILXorRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILXor rec
    MLIL_LSL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILLslRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILLsl rec
    MLIL_LSR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILLsrRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILLsr rec
    MLIL_ASR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILAsrRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILAsr rec
    MLIL_ROL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILRolRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILRol rec
    MLIL_RLC -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      carry' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILRlcRec
              { left = left',
                right = right',
                carry = carry',
                core = coreInst
              }
      pure $ Carry $ MediumLevelILRlc rec
    MLIL_ROR -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILRorRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILRor rec
    MLIL_RRC -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      carry' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILRrcRec
              { left = left',
                right = right',
                carry = carry',
                core = coreInst
              }
      pure $ Carry $ MediumLevelILRrc rec
    MLIL_MUL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILMulRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILMul rec
    MLIL_MULU_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILMuluDpRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ MediumLevelILMuluDp rec
    MLIL_MULS_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILMulsDpRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ MediumLevelILMulsDp rec
    MLIL_DIVU -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILDivuRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILDivu rec
    MLIL_DIVU_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILDivuDpRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ MediumLevelILDivuDp rec
    MLIL_DIVS -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILDivsRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILDivs rec
    MLIL_DIVS_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILDivsDpRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ MediumLevelILDivsDp rec
    MLIL_MODU -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILModuRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILModu rec
    MLIL_MODU_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILModuDpRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ MediumLevelILModuDp rec
    MLIL_MODS -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILModsRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILMods rec
    MLIL_MODS_DP -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILModsDpRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ MediumLevelILModsDp rec
    MLIL_NEG -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILNegRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILNeg rec
    MLIL_NOT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILNotRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILNot rec
    MLIL_SX -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILSxRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILSx rec
    MLIL_ZX -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILZxRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILZx rec
    MLIL_LOW_PART -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILLowPartRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILLowPart rec
    MLIL_JUMP -> do
      dest' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILJumpRec
              { dest = dest',
                core = coreInst
              }
      pure $ Terminal $ MediumLevelILJump rec
    MLIL_JUMP_TO -> do
      dest' <- getExpr func $ getOp rawInst 0
      target' <- getTargetMap func exprIndex' 1
      let rec =
            MediumLevelILJumpToRec
              { dest = dest',
                target = target',
                core = coreInst
              }
      pure $ Terminal $ MediumLevelILJumpTo rec
    MLIL_RET_HINT -> do
      dest' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILRetHintRec
              { dest = dest',
                core = coreInst
              }
      pure $ ControlFlow $ MediumLevelILRetHint rec
    MLIL_CALL -> do
      output' <- getVarList func exprIndex' 0
      dest' <- getExpr func $ getOp rawInst 2
      params' <- getExprList func exprIndex' 3
      let rec =
            MediumLevelILCallRec
              { output = output',
                dest = dest',
                params = params',
                core = coreInst
              }
      pure $ Localcall $ MediumLevelILCall rec
    MLIL_CALL_UNTYPED -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
        MediumLevelILCallOutput (MediumLevelILCallOutputRec {dest = d}) -> pure d
        _ ->
          error $
            "create: Output of MediumLevelILCallUntypedSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      paramsInst <- getExpr func $ getOp rawInst 2
      params' <- case paramsInst of
        MediumLevelILCallParam (MediumLevelILCallParamRec {src = s}) -> pure s
        _ ->
          error $
            "create: Params of MediumLevelILCallUntypedSsa: expected MediumLevelILCallParamsSsa : "
              ++ show outputInst
      stack' <- getExpr func $ getOp rawInst 3
      let rec =
            MediumLevelILCallUntypedRec
              { output = output',
                dest = dest',
                params = params',
                stack = stack',
                core = coreInst
              }
      pure $ Localcall $ MediumLevelILCallUntyped rec
    MLIL_CALL_OUTPUT -> do
      dest' <- getVarList func exprIndex' 0
      let rec =
            MediumLevelILCallOutputRec
              { dest = dest',
                core = coreInst
              }
      pure $ MediumLevelILCallOutput rec
    MLIL_CALL_PARAM -> do
      src' <- getExprList func exprIndex' 0
      let rec =
            MediumLevelILCallParamRec
              { src = src',
                core = coreInst
              }
      pure $ MediumLevelILCallParam rec
    MLIL_SEPARATE_PARAM_LIST -> do
      params' <- getExprList func exprIndex' 0
      let rec =
            MediumLevelILSeparateParamListRec
              { params = params',
                core = coreInst
              }
      pure $ MediumLevelILSeparateParamList rec
    MLIL_SHARED_PARAM_SLOT -> do
      params' <- getExprList func exprIndex' 0
      let rec =
            MediumLevelILSharedParamSlotRec
              { params = params',
                core = coreInst
              }
      pure $ MediumLevelILSharedParamSlot rec
    MLIL_RET -> do
      src' <- getExprList func exprIndex' 0
      let rec =
            MediumLevelILRetRec
              { src = src',
                core = coreInst
              }
      pure $ Return $ MediumLevelILRet rec
    MLIL_NORET -> do
      pure $ Terminal $ MediumLevelILNoret $ MediumLevelILNoretRec {core = coreInst}
    MLIL_IF -> do
      condition' <- getExpr func $ getOp rawInst 0
      true' <- getInt rawInst 1
      false' <- getInt rawInst 2
      let rec =
            MediumLevelILIfRec
              { condition = condition',
                true = true',
                false = false',
                core = coreInst
              }
      pure $ Terminal $ MediumLevelILIf rec
    MLIL_GOTO -> do
      dest' <- getInt rawInst 0
      let rec =
            MediumLevelILGotoRec
              { dest = dest',
                core = coreInst
              }
      pure $ Terminal $ MediumLevelILGoto rec
    MLIL_CMP_E -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpERec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpE rec
    MLIL_CMP_NE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpNeRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpNe rec
    MLIL_CMP_SLT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpSltRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpSlt rec
    MLIL_CMP_ULT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpUltRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpUlt rec
    MLIL_CMP_SLE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpSleRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpSle rec
    MLIL_CMP_ULE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpUleRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpUle rec
    MLIL_CMP_SGE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpSgeRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpSge rec
    MLIL_CMP_UGE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpUgeRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpUge rec
    MLIL_CMP_SGT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpSgtRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpSgt rec
    MLIL_CMP_UGT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILCmpUgtRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILCmpUgt rec
    MLIL_TEST_BIT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILTestBitRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILTestBit rec
    MLIL_BOOL_TO_INT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILBoolToIntRec
              { src = src',
                core = coreInst
              }
      pure $ MediumLevelILBoolToInt rec
    MLIL_ADD_OVERFLOW -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILAddOverflowRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILAddOverflow rec
    MLIL_SYSCALL -> do
      output' <- getVarList func exprIndex' 0
      params' <- getExprList func exprIndex' 2
      let rec =
            MediumLevelILSyscallRec
              { output = output',
                params = params',
                core = coreInst
              }
      pure $ Syscall $ MediumLevelILSyscall rec
    MLIL_SYSCALL_UNTYPED -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
        MediumLevelILCallOutput (MediumLevelILCallOutputRec {dest = d}) -> pure d
        _ ->
          error $
            "create: Output of MediumLevelILSyscallUntyped: expected MediumLevelILCallOutput : "
              ++ show outputInst
      paramInst <- getExpr func $ getOp rawInst 1
      params' <- case paramInst of
        MediumLevelILCallParam (MediumLevelILCallParamRec {src = s}) -> pure s
        _ ->
          error $
            "create: Params of MediumLevelILSyscallUntyped: expected MediumLevelILCallParam : "
              ++ show paramInst
      stack' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILSyscallUntypedRec
              { output = output',
                params = params',
                stack = stack',
                core = coreInst
              }
      pure $ Syscall $ MediumLevelILSyscallUntyped rec
    MLIL_TAILCALL -> do
      output' <- getVarList func exprIndex' 0
      dest' <- getExpr func $ getOp rawInst 2
      params' <- getExprList func exprIndex' 3
      let rec =
            MediumLevelILTailcallRec
              { output = output',
                dest = dest',
                params = params',
                core = coreInst
              }
      pure $ Tailcall $ MediumLevelILTailcall rec
    MLIL_TAILCALL_UNTYPED -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
        MediumLevelILCallOutput (MediumLevelILCallOutputRec {dest = d}) -> pure d
        _ ->
          error $
            "create: Output of MediumLevelILTailCallUntyped: expected MediumLevelILCallOutput : "
              ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      paramInst <- getExpr func $ getOp rawInst 2
      params' <- case paramInst of
        MediumLevelILCallParam (MediumLevelILCallParamRec {src = s}) -> pure s
        _ ->
          error $
            "create: Param of MediumLevelILTailCallUntyped: expected MediumLevelILCallParam : "
              ++ show paramInst
      stack' <- getExpr func $ getOp rawInst 3
      let rec =
            MediumLevelILTailcallUntypedRec
              { output = output',
                dest = dest',
                params = params',
                stack = stack',
                core = coreInst
              }
      pure $ Tailcall $ MediumLevelILTailcallUntyped rec
    MLIL_INTRINSIC -> do
      output' <- getVarList func exprIndex' 0
      intrinsic' <- getIntrinsicIL rawInst func 2
      params' <- getExprList func exprIndex' 3
      let rec =
            MediumLevelILIntrinsicRec
              { output = output',
                intrinsic = intrinsic',
                params = params',
                core = coreInst
              }
      pure $ IntrinsicInstruction $ MediumLevelILIntrinsic rec
    MLIL_FREE_VAR_SLOT -> do
      dest' <- varFromID $ fromIntegral $ getOp rawInst 0
      let rec =
            MediumLevelILFreeVarSlotRec
              { dest = dest',
                core = coreInst
              }
      pure $ RegisterStack $ MediumLevelILFreeVarSlot rec
    MLIL_BP -> do
      pure $ Terminal $ MediumLevelILBp $ MediumLevelILBpRec {core = coreInst}
    MLIL_TRAP -> do
      vector' <- getInt rawInst 0
      let rec =
            MediumLevelILTrapRec
              { vector = vector',
                core = coreInst
              }
      pure $ Terminal $ MediumLevelILTrap rec
    MLIL_UNDEF -> do
      pure $ MediumLevelILUndef $ MediumLevelILUndefRec {core = coreInst}
    MLIL_UNIMPL -> do
      pure $ MediumLevelILUnimpl $ MediumLevelILUnimplRec {core = coreInst}
    MLIL_UNIMPL_MEM -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILUnimplMemRec
              { src = src',
                core = coreInst
              }
      pure $ Memory $ MediumLevelILUnimplMem rec
    MLIL_FADD -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFaddRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFadd rec
    MLIL_FSUB -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFsubRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFsub rec
    MLIL_FMUL -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFmulRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFmul rec
    MLIL_FDIV -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFdivRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFdiv rec
    MLIL_FSQRT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFsqrtRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFsqrt rec
    MLIL_FNEG -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFnegRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFneg rec
    MLIL_FABS -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFabsRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFabs rec
    MLIL_FLOAT_TO_INT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFloatToIntRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFloatToInt rec
    MLIL_INT_TO_FLOAT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILIntToFloatRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILIntToFloat rec
    MLIL_FLOAT_CONV -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFloatConvRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFloatConv rec
    MLIL_ROUND_TO_INT -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILRoundToIntRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILRoundToInt rec
    MLIL_FLOOR -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFloorRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFloor rec
    MLIL_CEIL -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILCeilRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILCeil rec
    MLIL_FTRUNC -> do
      src' <- getExpr func $ getOp rawInst 0
      let rec =
            MediumLevelILFtruncRec
              { src = src',
                core = coreInst
              }
      pure $ Arithmetic $ MediumLevelILFtrunc rec
    MLIL_FCMP_E -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpERec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpE rec
    MLIL_FCMP_NE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpNeRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpNe rec
    MLIL_FCMP_LT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpLtRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpLt rec
    MLIL_FCMP_LE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpLeRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpLe rec
    MLIL_FCMP_GE -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpGeRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpGe rec
    MLIL_FCMP_GT -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpGtRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpGt rec
    MLIL_FCMP_O -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpORec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpO rec
    MLIL_FCMP_UO -> do
      left' <- getExpr func $ getOp rawInst 0
      right' <- getExpr func $ getOp rawInst 1
      let rec =
            MediumLevelILFcmpUoRec
              { left = left',
                right = right',
                core = coreInst
              }
      pure $ Comparison $ MediumLevelILFcmpUo rec
    MLIL_SET_VAR_SSA -> do
      dest' <- getSSAVar rawInst 0 1
      src' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILSetVarSsaRec
              { dest = dest',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarSsa rec
    MLIL_SET_VAR_SSA_FIELD -> do
      dest' <- getSSAVarAndDest rawInst 0 1
      prev' <- getSSAVarAndDest rawInst 0 2
      offset' <- getInt rawInst 3
      src' <- getExpr func $ getOp rawInst 4
      let rec =
            MediumLevelILSetVarSsaFieldRec
              { dest = dest',
                prev = prev',
                offset = offset',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarSsaField rec
    MLIL_SET_VAR_SPLIT_SSA -> do
      high' <- getSSAVar rawInst 0 1
      low' <- getSSAVar rawInst 2 3
      src' <- getExpr func $ getOp rawInst 4
      let rec =
            MediumLevelILSetVarSplitSsaRec
              { high = high',
                low = low',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarSplitSsa rec
    MLIL_SET_VAR_ALIASED -> do
      dest' <- getSSAVarAndDest rawInst 0 1
      prev' <- getSSAVarAndDest rawInst 0 2
      src' <- getExpr func $ getOp rawInst 3
      let rec =
            MediumLevelILSetVarAliasedRec
              { dest = dest',
                prev = prev',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarAliased rec
    MLIL_SET_VAR_ALIASED_FIELD -> do
      dest' <- getSSAVarAndDest rawInst 0 1
      prev' <- getSSAVarAndDest rawInst 0 2
      offset' <- getInt rawInst 3
      src' <- getExpr func $ getOp rawInst 4
      let rec =
            MediumLevelILSetVarAliasedFieldRec
              { dest = dest',
                prev = prev',
                offset = offset',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILSetVarAliasedField rec
    MLIL_VAR_SSA -> do
      src' <- getSSAVar rawInst 0 1
      let rec =
            MediumLevelILVarSsaRec
              { src = src',
                var = src',
                core = coreInst
              }
      pure $ VariableInstruction $ MediumLevelILVarSsa rec
    MLIL_VAR_SSA_FIELD -> do
      src' <- getSSAVar rawInst 0 1
      offset' <- getInt rawInst 2
      let rec =
            MediumLevelILVarSsaFieldRec
              { src = src',
                offset = offset',
                core = coreInst
              }
      pure $ VariableInstruction $ MediumLevelILVarSsaField rec
    MLIL_VAR_ALIASED -> do
      src' <- getSSAVar rawInst 0 1
      let rec =
            MediumLevelILVarAliasedRec
              { src = src',
                core = coreInst
              }
      pure $ VariableInstruction $ MediumLevelILVarAliased rec
    MLIL_VAR_ALIASED_FIELD -> do
      src' <- getSSAVar rawInst 0 1
      offset' <- getInt rawInst 2
      let rec =
            MediumLevelILVarAliasedFieldRec
              { src = src',
                offset = offset',
                core = coreInst
              }
      pure $ VariableInstruction $ MediumLevelILVarAliasedField rec
    MLIL_VAR_SPLIT_SSA -> do
      high' <- getSSAVar rawInst 0 1
      low' <- getSSAVar rawInst 2 3
      let rec =
            MediumLevelILVarSplitSsaRec
              { high = high',
                low = low',
                core = coreInst
              }
      pure $ VariableInstruction $ MediumLevelILVarSplitSsa rec
    MLIL_ASSERT_SSA -> do
      src' <- getSSAVar rawInst 0 1
      constraint' <- getConstraint func rawInst 2
      let rec =
            MediumLevelILAssertSsaRec
              { src = src',
                constraint = constraint',
                core = coreInst
              }
      pure $ MediumLevelILAssertSsa rec
    MLIL_FORCE_VER_SSA -> do
      dest' <- getSSAVar rawInst 0 1
      src' <- getSSAVar rawInst 2 3
      let rec =
            MediumLevelILForceVerSsaRec
              { dest = dest',
                src = src',
                core = coreInst
              }
      pure $ MediumLevelILForceVerSsa rec
    MLIL_CALL_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {dest = d}) -> pure d
        _ ->
          error $
            "create: Output of MediumLevelILCallSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      params' <- getExprList func exprIndex' 2
      srcMemory' <- getInt rawInst 4
      let rec =
            MediumLevelILCallSsaRec
              { output = output',
                dest = dest',
                params = params',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ Localcall $ MediumLevelILCallSsa rec
    MLIL_CALL_UNTYPED_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      output' <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {dest = d}) -> pure d
        _ ->
          error $
            "create: Output of MediumLevelILCallUntypedSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      outputDestMemory' <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {destMemory = d}) -> pure d
        _ ->
          error $
            "create: Output of MediumLevelILCallUntypedSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      paramsInst <- getExpr func $ getOp rawInst 2
      (params', paramsSrcMemory') <- case paramsInst of
        MediumLevelILCallParamSsa (MediumLevelILCallParamSsaRec {src = s', srcMemory = sm'}) -> pure (s', sm')
        _ ->
          error $
            "create: Params of MediumLevelILCallUntypedSsa: expected MediumLevelILCallParamsSsa : "
              ++ show outputInst
      stack' <- getExpr func $ getOp rawInst 3
      let rec =
            MediumLevelILCallUntypedSsaRec
              { output = output',
                outputDestMemory = outputDestMemory',
                dest = dest',
                params = params',
                paramsSrcMemory = paramsSrcMemory',
                stack = stack',
                core = coreInst
              }
      pure $ Localcall $ MediumLevelILCallUntypedSsa rec
    MLIL_SYSCALL_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      (output', outputDestMemory') <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {dest = d, destMemory = dm}) -> pure (d, dm)
        _ ->
          error $
            "create: Output of MediumLevelILSyscallSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      params' <- getExprList func exprIndex' 1
      srcMemory' <- getInt rawInst 3
      let rec =
            MediumLevelILSyscallSsaRec
              { output = output',
                outputDestMemory = outputDestMemory',
                params = params',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ Syscall $ MediumLevelILSyscallSsa rec
    MLIL_SYSCALL_UNTYPED_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      (output', outputDestMemory') <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {dest = d, destMemory = dm}) -> pure (d, dm)
        _ ->
          error $
            "create: Output of MediumLevelILSyscallUntypedSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      paramInst <- getExpr func $ getOp rawInst 1
      (params', paramsSrcMemory') <- case paramInst of
        MediumLevelILCallParamSsa (MediumLevelILCallParamSsaRec {src = p, srcMemory = psm}) -> pure (p, psm)
        _ ->
          error $
            "create: Params of MediumLevelILCallOutputSsa: expected MediumLevelILCallParamSsa : "
              ++ show paramInst
      stack' <- getExpr func $ getOp rawInst 2
      let rec =
            MediumLevelILSyscallUntypedSsaRec
              { output = output',
                outputDestMemory = outputDestMemory',
                params = params',
                paramsSrcMemory = paramsSrcMemory',
                stack = stack',
                core = coreInst
              }
      pure $ Syscall $ MediumLevelILSyscallUntypedSsa rec
    MLIL_TAILCALL_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      (output', outputDestMemory') <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {dest = d, destMemory = dM}) -> pure (d, dM)
        _ ->
          error $
            "create: Output of MediumLevelILTailcallSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      params' <- getExprList func exprIndex' 2
      srcMemory' <- getInt rawInst 4
      let rec =
            MediumLevelILTailcallSsaRec
              { output = output',
                outputDestMemory = outputDestMemory',
                dest = dest',
                params = params',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ Tailcall $ MediumLevelILTailcallSsa rec
    MLIL_TAILCALL_UNTYPED_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      (output', outputDestMemory') <- case outputInst of
        MediumLevelILCallOutputSsa (MediumLevelILCallOutputSsaRec {dest = d, destMemory = dM}) -> pure (d, dM)
        _ ->
          error $
            "create: Output of MediumLevelILTailcallSsa: expected MediumLevelILCallOutputSsa : "
              ++ show outputInst
      dest' <- getExpr func $ getOp rawInst 1
      paramInst <- getExpr func $ getOp rawInst 2
      params' <- case paramInst of
        MediumLevelILCallParamSsa (MediumLevelILCallParamSsaRec {src = p}) -> pure p
        _ ->
          error $
            "create: Params of MediumLevelILCallOutputSsa: expected MediumLevelILCallParamSsa : "
              ++ show paramInst
      stack' <- getExpr func $ getOp rawInst 3
      let rec =
            MediumLevelILTailcallUntypedSsaRec
              { output = output',
                outputDestMemory = outputDestMemory',
                dest = dest',
                params = params',
                stack = stack',
                core = coreInst
              }
      pure $ Tailcall $ MediumLevelILTailcallUntypedSsa rec
    MLIL_CALL_PARAM_SSA -> do
      srcMemory' <- getInt rawInst 0
      src' <- getExprList func exprIndex' 1
      let rec =
            MediumLevelILCallParamSsaRec
              { srcMemory = srcMemory',
                src = src',
                core = coreInst
              }
      pure $ MediumLevelILCallParamSsa rec
    MLIL_CALL_OUTPUT_SSA -> do
      destMemory' <- getInt rawInst 0
      dest' <- getSSAVarList func exprIndex' 1
      let rec =
            MediumLevelILCallOutputSsaRec
              { destMemory = destMemory',
                dest = dest',
                core = coreInst
              }
      pure $ MediumLevelILCallOutputSsa rec
    MLIL_MEMORY_INTRINSIC_OUTPUT_SSA -> do
      destMemory' <- getInt rawInst 0
      output' <- getSSAVarList func exprIndex' 1
      let rec =
            MediumLevelILMemoryIntrinsicOutputSsaRec
              { destMemory = destMemory',
                output = output',
                core = coreInst
              }
      pure $ MediumLevelILMemoryIntrinsicOutputSsa rec
    MLIL_LOAD_SSA -> do
      src' <- getExpr func $ getOp rawInst 0
      srcMemory' <- getInt rawInst 1
      let rec =
            MediumLevelILLoadSsaRec
              { src = src',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ Load $ MediumLevelILLoadSsa rec
    MLIL_LOAD_STRUCT_SSA -> do
      src' <- getExpr func $ getOp rawInst 0
      offset' <- getInt rawInst 1
      srcMemory' <- getInt rawInst 2
      let rec =
            MediumLevelILLoadStructSsaRec
              { src = src',
                offset = offset',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ Load $ MediumLevelILLoadStructSsa rec
    MLIL_STORE_SSA -> do
      dest' <- getExpr func $ getOp rawInst 0
      destMemory' <- getInt rawInst 1
      srcMemory' <- getInt rawInst 2
      src' <- getExpr func $ getOp rawInst 3
      let rec =
            MediumLevelILStoreSsaRec
              { dest = dest',
                destMemory = destMemory',
                srcMemory = srcMemory',
                src = src',
                core = coreInst
              }
      pure $ Store $ MediumLevelILStoreSsa rec
    MLIL_STORE_STRUCT_SSA -> do
      dest' <- getExpr func $ getOp rawInst 0
      offset' <- getInt rawInst 1
      destMemory' <- getInt rawInst 2
      srcMemory' <- getInt rawInst 3
      src' <- getExpr func $ getOp rawInst 4
      let rec =
            MediumLevelILStoreStructSsaRec
              { dest = dest',
                offset = offset',
                destMemory = destMemory',
                srcMemory = srcMemory',
                src = src',
                core = coreInst
              }
      pure $ Store $ MediumLevelILStoreStructSsa rec
    MLIL_INTRINSIC_SSA -> do
      output' <- getSSAVarList func exprIndex' 0
      intrinsic' <- getIntrinsicIL rawInst func 2
      params' <- getExprList func exprIndex' 3
      let rec =
            MediumLevelILIntrinsicSsaRec
              { output = output',
                intrinsic = intrinsic',
                params = params',
                core = coreInst
              }
      pure $ IntrinsicInstruction $ MediumLevelILIntrinsicSsa rec
    MLIL_MEMORY_INTRINSIC_SSA -> do
      outputInst <- getExpr func $ getOp rawInst 0
      (output', outputDestMemory') <- case outputInst of
        MediumLevelILMemoryIntrinsicOutputSsa (MediumLevelILMemoryIntrinsicOutputSsaRec {output = d, destMemory = dM}) -> pure (d, dM)
        _ ->
          error $
            "create: Output of MediumLevelILMemoryIntrinsicOutputSsa: expected MediumLevelILMemoryIntrinsicOutputSsa : "
              ++ show outputInst
      intrinsic' <- getIntrinsicIL rawInst func 1
      params' <- getExprList func exprIndex' 2
      srcMemory' <- getInt rawInst 4
      let rec =
            MediumLevelILMemoryIntrinsicSsaRec
              { output = output',
                destMemory = outputDestMemory',
                intrinsic = intrinsic',
                params = params',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ IntrinsicInstruction $ MediumLevelILMemoryIntrinsicSsa rec
    MLIL_FREE_VAR_SLOT_SSA -> do
      dest' <- getSSAVarAndDest rawInst 0 1
      prev' <- getSSAVarAndDest rawInst 0 2
      let rec =
            MediumLevelILFreeVarSlotSsaRec
              { dest = dest',
                prev = prev',
                core = coreInst
              }
      pure $ RegisterStack $ MediumLevelILFreeVarSlotSsa rec
    MLIL_VAR_PHI -> do
      dest' <- getSSAVar rawInst 0 1
      src' <- getSSAVarList func exprIndex' 2
      let rec =
            MediumLevelILVarPhiRec
              { dest = dest',
                src = src',
                core = coreInst
              }
      pure $ SetVar $ MediumLevelILVarPhi rec
    MLIL_MEM_PHI -> do
      destMemory' <- getInt rawInst 0
      srcMemory' <- getIntList func exprIndex' 1
      let rec =
            MediumLevelILMemPhiRec
              { destMemory = destMemory',
                srcMemory = srcMemory',
                core = coreInst
              }
      pure $ Memory $ MediumLevelILMemPhi rec

-- | Deconstructs the provided instruction to derive the list of all child instructions.
children :: MediumLevelILSSAInstruction -> [MediumLevelILSSAInstruction]
children (Localcall lc) =
  case lc of
    MediumLevelILCall MediumLevelILCallRec {dest = d, params = p} ->
      concatMap children p ++ children d ++ p ++ [d]
    MediumLevelILCallSsa MediumLevelILCallSsaRec {dest = d, params = p} ->
      concatMap children p ++ children d ++ p ++ [d]
    MediumLevelILCallUntyped MediumLevelILCallUntypedRec {dest = d, params = p} ->
      concatMap children p ++ children d ++ p ++ [d]
    MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec {dest = d, params = p} ->
      concatMap children p ++ children d ++ p ++ [d]
children (Constant _) = []
children (Comparison cmp) =
  case cmp of
    MediumLevelILCmpE MediumLevelILCmpERec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpE MediumLevelILFcmpERec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpNe MediumLevelILCmpNeRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpNe MediumLevelILFcmpNeRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpLt MediumLevelILFcmpLtRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpLe MediumLevelILFcmpLeRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpGe MediumLevelILFcmpGeRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpGt MediumLevelILFcmpGtRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpSlt MediumLevelILCmpSltRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpUlt MediumLevelILCmpUltRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpSle MediumLevelILCmpSleRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpUle MediumLevelILCmpUleRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpSge MediumLevelILCmpSgeRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpUge MediumLevelILCmpUgeRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpSgt MediumLevelILCmpSgtRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILCmpUgt MediumLevelILCmpUgtRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpO MediumLevelILFcmpORec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFcmpUo MediumLevelILFcmpUoRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILTestBit MediumLevelILTestBitRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
children (Arithmetic a) =
  case a of
    MediumLevelILNeg MediumLevelILNegRec {src = s} -> children s ++ [s]
    MediumLevelILNot MediumLevelILNotRec {src = s} -> children s ++ [s]
    MediumLevelILSx MediumLevelILSxRec {src = s} -> children s ++ [s]
    MediumLevelILZx MediumLevelILZxRec {src = s} -> children s ++ [s]
    MediumLevelILLowPart MediumLevelILLowPartRec {src = s} -> children s ++ [s]
    MediumLevelILFsqrt MediumLevelILFsqrtRec {src = s} -> children s ++ [s]
    MediumLevelILFneg MediumLevelILFnegRec {src = s} -> children s ++ [s]
    MediumLevelILFabs MediumLevelILFabsRec {src = s} -> children s ++ [s]
    MediumLevelILFloatToInt MediumLevelILFloatToIntRec {src = s} -> children s ++ [s]
    MediumLevelILIntToFloat MediumLevelILIntToFloatRec {src = s} -> children s ++ [s]
    MediumLevelILFloatConv MediumLevelILFloatConvRec {src = s} -> children s ++ [s]
    MediumLevelILRoundToInt MediumLevelILRoundToIntRec {src = s} -> children s ++ [s]
    MediumLevelILFloor MediumLevelILFloorRec {src = s} -> children s ++ [s]
    MediumLevelILCeil MediumLevelILCeilRec {src = s} -> children s ++ [s]
    MediumLevelILFtrunc MediumLevelILFtruncRec {src = s} -> children s ++ [s]
    MediumLevelILAdd MediumLevelILAddRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILSub MediumLevelILSubRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILAnd MediumLevelILAndRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILOr MediumLevelILOrRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILXor MediumLevelILXorRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILLsl MediumLevelILLslRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILLsr MediumLevelILLsrRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILAsr MediumLevelILAsrRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILRol MediumLevelILRolRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILRor MediumLevelILRorRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILMul MediumLevelILMulRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILDivu MediumLevelILDivuRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILDivs MediumLevelILDivsRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILModu MediumLevelILModuRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILMods MediumLevelILModsRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILAddOverflow MediumLevelILAddOverflowRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFadd MediumLevelILFaddRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFsub MediumLevelILFsubRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFmul MediumLevelILFmulRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
    MediumLevelILFdiv MediumLevelILFdivRec {left = l, right = r} ->
      children l ++ children r ++ [l, r]
children (Terminal t) =
  case t of
    MediumLevelILNoret _ -> []
    MediumLevelILBp _ -> []
    MediumLevelILJump MediumLevelILJumpRec {dest = d} -> children d ++ [d]
    MediumLevelILGoto _ -> []
    MediumLevelILTrap _ -> []
    MediumLevelILJumpTo MediumLevelILJumpToRec {dest = d} -> children d ++ [d]
    MediumLevelILIf MediumLevelILIfRec {condition = c} -> children c ++ [c]
children (Syscall s) =
  case s of
    MediumLevelILSyscallUntyped MediumLevelILSyscallUntypedRec {params = p} ->
      concatMap children p ++ p
    MediumLevelILSyscallSsa MediumLevelILSyscallSsaRec {params = p} ->
      concatMap children p ++ p
    MediumLevelILSyscall MediumLevelILSyscallRec {params = p} ->
      concatMap children p ++ p
    MediumLevelILSyscallUntypedSsa MediumLevelILSyscallUntypedSsaRec {params = p} ->
      concatMap children p ++ p
children (Tailcall t) =
  case t of
    MediumLevelILTailcallUntyped MediumLevelILTailcallUntypedRec {dest = d, params = p} ->
      children d ++ concatMap children p ++ p ++ [d]
    MediumLevelILTailcall MediumLevelILTailcallRec {dest = d, params = p} ->
      children d ++ concatMap children p ++ p ++ [d]
    MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec {dest = d, params = p} ->
      children d ++ concatMap children p ++ p ++ [d]
    MediumLevelILTailcallUntypedSsa MediumLevelILTailcallUntypedSsaRec {dest = d, params = p} ->
      children d ++ concatMap children p ++ p ++ [d]
children (ControlFlow (MediumLevelILRetHint MediumLevelILRetHintRec {dest = d})) =
  children d ++ [d]
children (Return (MediumLevelILRet MediumLevelILRetRec {src = s})) =
  concatMap children s ++ s
children (Load l) =
  case l of
    MediumLevelILLoad MediumLevelILLoadRec {src = s} ->
      children s ++ [s]
    MediumLevelILLoadStruct MediumLevelILLoadStructRec {src = s} ->
      children s ++ [s]
    MediumLevelILLoadSsa MediumLevelILLoadSsaRec {src = s} ->
      children s ++ [s]
    MediumLevelILLoadStructSsa MediumLevelILLoadStructSsaRec {src = s} ->
      children s ++ [s]
children (Store store') =
  case store' of
    MediumLevelILStore MediumLevelILStoreRec {src = s, dest = d} ->
      children s ++ children d ++ [s, d]
    MediumLevelILStoreStruct MediumLevelILStoreStructRec {src = s, dest = d} ->
      children s ++ children d ++ [s, d]
    MediumLevelILStoreSsa MediumLevelILStoreSsaRec {src = s, dest = d} ->
      children s ++ children d ++ [s, d]
    MediumLevelILStoreStructSsa MediumLevelILStoreStructSsaRec {src = s, dest = d} ->
      children s ++ children d ++ [s, d]
children (Memory m) =
  case m of
    MediumLevelILUnimplMem MediumLevelILUnimplMemRec {src = s} ->
      children s ++ [s]
    MediumLevelILMemPhi _ -> []
children (Carry carry') =
  case carry' of
    MediumLevelILAdc MediumLevelILAdcRec {left = l, right = r, carry = c} ->
      children l ++ children r ++ children c ++ [l, r, c]
    MediumLevelILSbb MediumLevelILSbbRec {left = l, right = r, carry = c} ->
      children l ++ children r ++ children c ++ [l, r, c]
    MediumLevelILRlc MediumLevelILRlcRec {left = l, right = r, carry = c} ->
      children l ++ children r ++ children c ++ [l, r, c]
    MediumLevelILRrc MediumLevelILRrcRec {left = l, right = r, carry = c} ->
      children l ++ children r ++ children c ++ [l, r, c]
children (SetVar sv) =
  case sv of
    MediumLevelILSetVar MediumLevelILSetVarRec {src = s} ->
      children s ++ [s]
    MediumLevelILVarPhi _ -> []
    MediumLevelILSetVarSsa MediumLevelILSetVarSsaRec {src = s} ->
      children s ++ [s]
    MediumLevelILSetVarAliased MediumLevelILSetVarAliasedRec {src = s} ->
      children s ++ [s]
    MediumLevelILSetVarSsaField MediumLevelILSetVarSsaFieldRec {src = s} ->
      children s ++ [s]
    MediumLevelILSetVarSplitSsa MediumLevelILSetVarSplitSsaRec {src = s} ->
      children s ++ [s]
    MediumLevelILSetVarAliasedField MediumLevelILSetVarAliasedFieldRec {src = s} ->
      children s ++ [s]
    MediumLevelILSetVarField MediumLevelILSetVarFieldRec {src = s} ->
      children s ++ [s]
    MediumLevelILSetVarSplit MediumLevelILSetVarSplitRec {src = s} ->
      children s ++ [s]
children (RegisterStack _) = []
children (VariableInstruction _) = []
children (IntrinsicInstruction ii) =
  case ii of
    MediumLevelILIntrinsic MediumLevelILIntrinsicRec {params = p} ->
      concatMap children p ++ p
    MediumLevelILIntrinsicSsa MediumLevelILIntrinsicSsaRec {params = p} ->
      concatMap children p ++ p
    MediumLevelILMemoryIntrinsicSsa MediumLevelILMemoryIntrinsicSsaRec {params = p} ->
      concatMap children p ++ p
children (MediumLevelILCallOutputSsa _) = []
children (MediumLevelILCallOutput _) = []
children (MediumLevelILMemoryIntrinsicOutputSsa _) = []
children (MediumLevelILCallParamSsa MediumLevelILCallParamSsaRec {src = s}) =
  concatMap children s ++ s
children (MediumLevelILCallParam MediumLevelILCallParamRec {src = s}) =
  concatMap children s ++ s
children (MediumLevelILNop _) = []
children (MediumLevelILAddressOf _) = []
children (MediumLevelILAddressOfField _) = []
children (MediumLevelILMuluDp MediumLevelILMuluDpRec {left = l, right = r}) =
  children l ++ children r ++ [l, r]
children (MediumLevelILMulsDp MediumLevelILMulsDpRec {left = l, right = r}) =
  children l ++ children r ++ [l, r]
children (MediumLevelILDivuDp MediumLevelILDivuDpRec {left = l, right = r}) =
  children l ++ children r ++ [l, r]
children (MediumLevelILDivsDp MediumLevelILDivsDpRec {left = l, right = r}) =
  children l ++ children r ++ [l, r]
children (MediumLevelILModuDp MediumLevelILModuDpRec {left = l, right = r}) =
  children l ++ children r ++ [l, r]
children (MediumLevelILModsDp MediumLevelILModsDpRec {left = l, right = r}) =
  children l ++ children r ++ [l, r]
children (MediumLevelILBoolToInt MediumLevelILBoolToIntRec {src = s}) =
  children s ++ [s]
children (MediumLevelILAssert _) = []
children (MediumLevelILAssertSsa _) = []
children (MediumLevelILForceVer _) = []
children (MediumLevelILForceVerSsa _) = []
children (MediumLevelILVarField _) = []
children (MediumLevelILVarSplit _) = []
children (MediumLevelILUndef _) = []
children (MediumLevelILUnimpl _) = []
children (MediumLevelILSeparateParamList MediumLevelILSeparateParamListRec {params = p}) =
  concatMap children p ++ p
children (MediumLevelILSharedParamSlot MediumLevelILSharedParamSlotRec {params = p}) =
  concatMap children p ++ p
