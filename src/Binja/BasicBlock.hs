{-# LANGUAGE DuplicateRecordFields #-}

module Binja.BasicBlock
  ( Binja.BasicBlock.fromMlilFunction,
    Binja.BasicBlock.fromMlilSSAFunction,
    Binja.BasicBlock.outgoingEdges,
    Binja.BasicBlock.incomingEdges,
    Binja.BasicBlock.fromBlockPtr,
    Binja.BasicBlock.fromBlockEdge,
  )
where

-- implement BasicBlockMlilSSA -> dominators etc

import Binja.FFI
import Binja.Types (BNBasicBlockEdge (..), BNBasicBlockPtr, BNMlilFunctionPtr, BNMlilSSAFunctionPtr, BasicBlockEdge (..), BasicBlockMlilSSA (..), Ptr, alloca, castPtr, nullPtr, peek, peekArray)
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

fromBlockPtr :: BNBasicBlockPtr -> IO BasicBlockMlilSSA
fromBlockPtr blockPtr = do
  startInstructionIndex <- c_BNGetBasicBlockStart blockPtr
  endInstructionIndex <- c_BNGetBasicBlockEnd blockPtr
  canExit' <- c_BNBasicBlockCanExit blockPtr -- CBool to Bool
  hasInvalidInstructions' <- c_BNBasicBlockHasInvalidInstructions blockPtr -- CBool to Bool
  pure $
    BasicBlockMlilSSA
      { start = fromIntegral startInstructionIndex,
        end = fromIntegral endInstructionIndex - 1,
        canExit = toBool canExit',
        hasInvalidInstructions = toBool hasInvalidInstructions'
      }

fromBlockEdge :: BNBasicBlockEdge -> BasicBlockEdge
fromBlockEdge
  BNBasicBlockEdge
    { ty = edgeTy,
      backEdge = backEdge',
      fallThrough = fallThrough'
    } =
    BasicBlockEdge
      { ty = edgeTy,
        backEdge = Binja.Utils.toBool backEdge',
        fallThrough = Binja.Utils.toBool fallThrough'
      }
