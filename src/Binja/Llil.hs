module Binja.Llil
  ( Binja.Llil.startIndex,
    Binja.Llil.sourceFunc,
    Binja.Llil.fromRef,
    Binja.Llil.at,
    Binja.Llil.llilRefToMlilExprIndex,
  )
where

import Binja.BinaryView (functionsContaining)
import Binja.FFI
import Binja.Function
import Binja.Types

sourceFunc :: BNLlilFunctionPtr -> IO BNFunctionPtr
sourceFunc func = do
  c_BNGetLowLevelILOwnderFunction func

startIndex :: BNLlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize
startIndex func arch' addr = do
  if arch' == nullPtr || func == nullPtr
    then error "startIndex: called with nullPtr argument"
    else do
      startI <- c_BNLowLevelILGetInstructionStart func arch' addr
      count <- c_BNGetLowLevelILInstructionCount func
      -- Ensure start index is less than total llil instructions
      -- in function
      if startI >= count
        then error ("startIndex: startI:" ++ show startI ++ " >= count:" ++ show count)
        else pure startI

-- Convert an instruction index into an expression index
instIndexToExprIndex :: BNLlilFunctionPtr -> Word64 -> IO CSize
instIndexToExprIndex = c_BNGetLowLevelILIndexForInstruction

llilByIndex :: BNLlilFunctionPtr -> CSize -> IO BNLowLevelILInstruction
llilByIndex func index' =
  alloca $ \p -> do
    _ <- c_BNGetLowLevelILByIndexPtr p func index'
    peek p

-- Retrieve the best LLIL instruction for the address in BNReferenceSource
fromRef :: BNReferenceSource -> IO BNLowLevelILInstruction
fromRef ref = do
  func <- Binja.Function.llil (bnFunc ref)
  sIndex <- startIndex func (bnArch ref) (bnAddr ref)
  exprIndex' <- instIndexToExprIndex func (fromIntegral sIndex)
  llilByIndex func exprIndex'

at :: BNBinaryViewPtr -> Word64 -> IO BNLowLevelILInstruction
at view addr = do
  rawFuncs <- functionsContaining view addr
  if null rawFuncs
    then error $ "No functions at: " ++ show addr
    else do
      llilFunc <- Binja.Function.llil $ head rawFuncs
      sIndex <- startIndex llilFunc (Binja.Function.architecture $ head rawFuncs) addr
      exprIndex' <- instIndexToExprIndex llilFunc (fromIntegral sIndex)
      llilByIndex llilFunc exprIndex'

-- Retrieve the best LLIL instruction for the address in BNReferenceSource
llilRefToMlilExprIndex :: BNReferenceSource -> IO CSize
llilRefToMlilExprIndex ref = do
  func <- Binja.Function.llil (bnFunc ref)
  sIndex <- startIndex func (bnArch ref) (bnAddr ref)
  llilExprIndex <- instIndexToExprIndex func (fromIntegral sIndex)
  mlilExprIndex <- c_BNGetMediumLevelILExprIndex func llilExprIndex
  funcSSA <- Binja.Function.mlil (bnFunc ref)
  c_BNGetMediumLevelILSSAExprIndex funcSSA mlilExprIndex
