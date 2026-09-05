{-# LANGUAGE DuplicateRecordFields #-}

module Binja.BasicBlock
  ( Binja.BasicBlock.fromMlilFunction,
    Binja.BasicBlock.fromMlilSSAFunction,
    Binja.BasicBlock.outgoingEdges,
    Binja.BasicBlock.incomingEdges,
    Binja.BasicBlock.fromBlockPtr,
    Binja.BasicBlock.fromBlockEdge,
    Binja.BasicBlock.contains,
  )
where

import Binja.FFI
import Binja.Mlil (create)
import Binja.Types.Core (BNBasicBlockEdge (..), BNBasicBlockPtr, BNMlilFunctionPtr, BNMlilSSAFunctionPtr, BasicBlockEdge (..), BasicBlockMlilSSA (..), Ptr, Word64, alloca, castPtr, nullPtr, peek, peekArray)
import Binja.Utils (toBool)

fromMlilFunction :: BNMlilFunctionPtr -> IO [BNBasicBlockPtr]
fromMlilFunction func = do
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetMediumLevelILBasicBlockList func countPtr
    count' <- peek countPtr
    if arrPtr == nullPtr || count' == 0
      then error "basicBlocks: arrPtr null or count is 0"
      else do
        refs <- peekArray (fromIntegral count') (castPtr arrPtr :: Ptr BNBasicBlockPtr)
        c_BNFreeBasicBlockList arrPtr count'
        pure refs

fromMlilSSAFunction :: BNMlilSSAFunctionPtr -> IO [BNBasicBlockPtr]
fromMlilSSAFunction func = do
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetMediumLevelILSSABasicBlockList func countPtr
    count' <- peek countPtr
    if arrPtr == nullPtr || count' == 0
      then error "basicBlocks: arrPtr null or count is 0"
      else do
        refs <- peekArray (fromIntegral count') (castPtr arrPtr :: Ptr BNBasicBlockPtr)
        c_BNFreeBasicBlockList arrPtr count'
        pure refs

outgoingEdges :: BNBasicBlockPtr -> IO [BNBasicBlockEdge]
outgoingEdges blockPtr = do
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetBasicBlockOutgoingEdges blockPtr countPtr
    count' <- peek countPtr
    edges <- peekArray (fromIntegral count') (castPtr arrPtr :: Ptr BNBasicBlockEdge)
    c_BNFreeBasicBlockEdgeList arrPtr count'
    pure edges

incomingEdges :: BNBasicBlockPtr -> IO [BNBasicBlockEdge]
incomingEdges blockPtr = do
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetBasicBlockIncomingEdges blockPtr countPtr
    count' <- peek countPtr
    edges <- peekArray (fromIntegral count') (castPtr arrPtr :: Ptr BNBasicBlockEdge)
    c_BNFreeBasicBlockEdgeList arrPtr count'
    pure edges

fromBlockPtr :: BNMlilSSAFunctionPtr -> BNBasicBlockPtr -> IO BasicBlockMlilSSA
fromBlockPtr funcPtr blockPtr = do
  startInstructionIndex <- fromIntegral <$> c_BNGetBasicBlockStart blockPtr
  endInstructionIndex <- fromIntegral <$> c_BNGetBasicBlockEnd blockPtr
  sourceBasicBlockPtr <- c_BNGetBasicBlockSource blockPtr
  startAddress' <- fromIntegral <$> c_BNGetBasicBlockStart sourceBasicBlockPtr
  endAddress' <- fromIntegral <$> c_BNGetBasicBlockEnd sourceBasicBlockPtr
  exprs' <- mapM (c_BNGetMediumLevelILSSAIndexForInstruction funcPtr) [startInstructionIndex .. endInstructionIndex - 1]
  instructions' <- mapM (Binja.Mlil.create funcPtr) exprs'
  canExit' <- c_BNBasicBlockCanExit blockPtr -- CBool to Bool
  hasInvalidInstructions' <- c_BNBasicBlockHasInvalidInstructions blockPtr -- CBool to Bool
  pure $
    BasicBlockMlilSSA
      { handle = blockPtr,
        startAddress = startAddress',
        endAddress = endAddress',
        instructions = instructions',
        canExit = toBool canExit',
        hasInvalidInstructions = toBool hasInvalidInstructions'
      }

fromBlockEdge :: BNMlilSSAFunctionPtr -> BNBasicBlockEdge -> IO BasicBlockEdge
fromBlockEdge
  funcPtr
  BNBasicBlockEdge
    { ty = edgeTy,
      target = target',
      backEdge = backEdge',
      fallThrough = fallThrough'
    } = do
    liftedBlock <- fromBlockPtr funcPtr target'
    pure
      BasicBlockEdge
        { ty = edgeTy,
          target = liftedBlock,
          backEdge = Binja.Utils.toBool backEdge',
          fallThrough = Binja.Utils.toBool fallThrough'
        }

-- | True if a block contains an address
contains :: BasicBlockMlilSSA -> Word64 -> Bool
contains BasicBlockMlilSSA {startAddress = start', endAddress = end'} address =
  address >= start' || address <= end'
