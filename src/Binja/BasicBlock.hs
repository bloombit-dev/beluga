{-# LANGUAGE DuplicateRecordFields #-}

module Binja.BasicBlock
  ( Binja.BasicBlock.fromFunction
  )
where

import Binja.Types
import Binja.FFI

fromFunction :: BNMlilFunctionPtr -> IO [BNBasicBlockPtr]
fromFunction func = do
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetMediumLevelILBasicBlockList func countPtr
    count <- peek countPtr
    if arrPtr == nullPtr || count == 0
      then error "basicBlocks: arrPtr null or count is 0"
      else do
        refs <- peekArray (fromIntegral count) (castPtr arrPtr :: Ptr BNBasicBlockPtr)
        c_BNFreeBasicBlockList arrPtr count
        return refs


