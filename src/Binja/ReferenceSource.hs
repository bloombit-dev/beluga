module Binja.ReferenceSource
  ( Binja.ReferenceSource.codeRefs,
  )
where

import Binja.FFI
import Binja.Types

data CodeReferenceList = CodeReferenceList
  { crArrayPtr :: !(ForeignPtr BNFunctionPtr),
    crCount :: !Int,
    crList :: ![BNReferenceSource],
    crViewPtr :: !BNBinaryViewPtr
  }
  deriving (Eq, Show)

codeRefs :: BNBinaryViewPtr -> Word64 -> IO [BNReferenceSource]
codeRefs view addr =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetCodeReferences view addr countPtr (CBool 0) 0
    count' <- fromIntegral <$> peek countPtr
    if arrPtr == nullPtr || count' == 0
      then pure []
      else do
        headPtr <- peekArray count' (castPtr arrPtr :: Ptr BNReferenceSource)
        c_BNFreeCodeReferences arrPtr (fromIntegral count')
        pure headPtr
