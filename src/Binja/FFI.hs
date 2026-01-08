{-# LANGUAGE ForeignFunctionInterface #-}

module Binja.FFI where

import Binja.Types

foreign import ccall unsafe "BNGetCachedMediumLevelILPossibleValueSetPtr"
  c_BNGetCachedMediumLevelILPossibleValueSetPtr ::
    Ptr BNPossibleValueSet -> BNMlilSSAFunctionPtr -> CSize -> IO (Ptr BNPossibleValueSet)

foreign import ccall unsafe "BNGetConstantData"
  c_BNGetConstantData ::
    BNFunctionPtr ->
    CSize ->
    CSize ->
    CSize ->
    Ptr CInt ->
    IO BNDataBufferPtr

foreign import ccall unsafe "BNGetCodeReferences"
  c_BNGetCodeReferences ::
    BNBinaryViewPtr ->
    Word64 ->
    Ptr CSize ->
    CBool ->
    CSize ->
    IO BNReferenceSourcePtr

foreign import ccall unsafe "BNFreeCodeReferences"
  c_BNFreeCodeReferences :: BNReferenceSourcePtr -> CSize -> IO ()

foreign import ccall "BNGetSymbolType"
  c_BNGetSymbolType :: BNSymbolPtr -> IO CInt

foreign import ccall "BNGetSymbolBinding"
  c_BNGetSymbolBinding :: BNSymbolPtr -> IO CInt

foreign import ccall "BNGetSymbolRawName"
  c_BNGetSymbolRawName :: BNSymbolPtr -> IO CString

foreign import ccall "BNGetSymbolShortName"
  c_BNGetSymbolShortName :: BNSymbolPtr -> IO CString

foreign import ccall "BNGetSymbolFullName"
  c_BNGetSymbolFullName :: BNSymbolPtr -> IO CString

foreign import ccall "BNGetSymbolAddress"
  c_BNGetSymbolAddress :: BNSymbolPtr -> IO Word64

foreign import ccall "BNGetSymbolOrdinal"
  c_BNGetSymbolOrdinal :: BNSymbolPtr -> IO CInt

foreign import ccall "BNIsSymbolAutoDefined"
  c_BNIsSymbolAutoDefined :: BNSymbolPtr -> IO CBool

foreign import ccall "BNGetSymbolByAddress"
  c_BNGetSymbolByAddress :: BNBinaryViewPtr -> Word64 -> BNNameSpacePtr -> IO BNSymbolPtr

foreign import ccall unsafe "BNMediumLevelILGetInstructionStart"
  c_BNMediumLevelILGetInstructionStart ::
    BNMlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILInstructionCount"
  c_BNGetMediumLevelILInstructionCount ::
    BNMlilFunctionPtr -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILInstructionCount"
  c_BNGetMediumLevelILSSAInstructionCount ::
    BNMlilSSAFunctionPtr -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILIndexForInstruction"
  c_BNGetMediumLevelILIndexForInstruction ::
    BNMlilFunctionPtr -> Word64 -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILIndexForInstruction"
  c_BNGetMediumLevelILSSAIndexForInstruction ::
    BNMlilSSAFunctionPtr -> Word64 -> IO CSize

foreign import ccall "BNGetMediumLevelILNonSSAInstructionIndex"
  c_BNGetMediumLevelILNonSSAInstructionIndex :: BNMlilSSAFunctionPtr -> CSize -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILSSAInstructionIndex"
  c_BNGetMediumLevelILSSAInstructionIndex ::
    BNMlilSSAFunctionPtr -> CSize -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILByIndexPtr"
  c_BNGetMediumLevelILByIndexPtr ::
    Ptr BNMediumLevelILInstruction -> BNMlilSSAFunctionPtr -> CSize -> IO (Ptr BNMediumLevelILInstruction)

foreign import ccall unsafe "BNGetMediumLevelSSAILByIndexPtr"
  c_BNGetMediumLevelSSAILByIndexPtr ::
    Ptr BNMediumLevelILInstruction -> BNMlilSSAFunctionPtr -> CSize -> IO (Ptr BNMediumLevelILInstruction)

foreign import ccall unsafe "BNGetMediumLevelILSSAExprIndex"
  c_BNGetMediumLevelILSSAExprIndex ::
    BNMlilFunctionPtr -> CSize -> IO CSize

foreign import ccall unsafe "BNGetMediumLevelILExprIndex"
  c_BNGetMediumLevelILExprIndex ::
    BNLlilFunctionPtr -> CSize -> IO CSize

foreign import ccall unsafe "BNMediumLevelILFreeOperandList"
  c_BNMediumLevelILFreeOperandList ::
    Ptr CULLong -> IO ()

foreign import ccall unsafe "BNMediumLevelILGetOperandList"
  c_BNMediumLevelILGetOperandList ::
    BNMlilSSAFunctionPtr -> CSize -> CSize -> Ptr CSize -> IO (Ptr CULLong)

foreign import ccall unsafe "BNFromVariableIdentifierPtr"
  c_BNFromVariableIdentifierPtr ::
    Ptr BNVariable -> CULLong -> IO ()

foreign import ccall unsafe "BNGetMediumLevelILBasicBlockList"
  c_BNGetMediumLevelILBasicBlockList ::
    BNMlilFunctionPtr -> Ptr CSize -> IO (Ptr BNBasicBlockPtr)

foreign import ccall unsafe "BNFreeBasicBlockList"
  c_BNFreeBasicBlockList ::
    Ptr BNBasicBlockPtr -> CSize -> IO ()

foreign import ccall "BNFreeBasicBlockEdgeList"
  c_BNFreeBasicBlockEdgeList :: BNBasicBlockEdgePtr -> CSize -> IO ()

foreign import ccall unsafe "BNGetBasicBlockStart"
  c_BNGetBasicBlockStart ::
    BNBasicBlockPtr -> IO CULLong

foreign import ccall unsafe "BNGetBasicBlockEnd"
  c_BNGetBasicBlockEnd ::
    BNBasicBlockPtr -> IO CULLong

foreign import ccall unsafe "BNGetBasicBlockFunction"
  c_BNGetBasicBlockFunction ::
    BNBasicBlockPtr -> IO BNFunctionPtr

foreign import ccall "BNGetProduct"
  c_BNGetProduct :: IO CString

--   BNBinaryView* BNLoadFilename(const char* const filename,
--                                const bool updateAnalysis,
--                                const char* options,
--                                BNProgressFunction progress,
--                                void* progressContext);
--
foreign import ccall "BNLoadFilename"
  c_BNLoadFilename ::
    CString -> -- filename
    CBool -> -- updateAnalysis flag
    CString -> -- options (e.g., JSON string)
    BNProgressFunctionPtr -> -- progress function pointer
    Ptr () -> -- progress context (can be null)
    IO BNBinaryViewPtr

foreign import ccall "BNHasFunctions"
  c_BNHasFunctions :: BNBinaryViewPtr -> IO CBool

foreign import ccall "BNHasSymbols"
  c_BNHasSymbols :: BNBinaryViewPtr -> CBool

foreign import ccall "BNHasDataVariables"
  c_BNHasDataVariables :: BNBinaryViewPtr -> CBool

-- bool BNSaveToFilename(BNBinaryView* view, const char* filename)
foreign import ccall "BNSaveToFilename"
  c_BNSaveToFilename :: BNBinaryViewPtr -> CString -> IO CBool

foreign import ccall "BNUpdateAnalysis"
  c_BNUpdateAnalysis :: BNBinaryViewPtr -> IO ()

foreign import ccall "BNUpdateAnalysisAndWait"
  c_BNUpdateAnalysisAndWait :: BNBinaryViewPtr -> IO ()

foreign import ccall "BNAbortAnalysis"
  c_BNAbortAnalysis :: BNBinaryViewPtr -> IO ()

foreign import ccall unsafe "BNGetAnalysisFunctionList"
  c_BNGetAnalysisFunctionList ::
    BNBinaryViewPtr ->
    Ptr CSize ->
    IO (Ptr BNFunctionPtr)

foreign import ccall unsafe "BNFreeFunctionList"
  c_BNFreeFunctionList :: Ptr BNFunctionPtr -> CSize -> IO ()

foreign import ccall unsafe "BNGetSymbols"
  c_BNGetSymbols ::
    BNBinaryViewPtr ->
    Ptr CSize ->
    BNNameSpacePtr ->
    IO (Ptr BNSymbolPtr)

foreign import ccall unsafe "BNFreeSymbolList"
  c_BNFreeSymbolList :: Ptr BNSymbolPtr -> CSize -> IO ()

foreign import ccall unsafe "BNGetAnalysisFunctionsContainingAddress"
  c_BNGetAnalysisFunctionsContainingAddress ::
    BNBinaryViewPtr ->
    Word64 ->
    Ptr CSize ->
    IO (Ptr BNFunctionPtr)

foreign import ccall unsafe "BNGetAnalysisFunctionsForAddress"
  c_BNGetAnalysisFunctionsForAddress ::
    BNBinaryViewPtr ->
    Word64 ->
    Ptr CSize ->
    IO (Ptr BNFunctionPtr)

foreign import ccall unsafe "BNGetStrings"
  c_BNGetStrings ::
    BNBinaryViewPtr ->
    Ptr CSize ->
    IO (Ptr BNStringRefPtr)

foreign import ccall unsafe "BNFreeStringReferenceList"
  c_BNFreeStringReferenceList :: Ptr BNStringRefPtr -> IO ()

foreign import ccall unsafe "BNReadViewBuffer"
  c_BNReadViewBuffer ::
    BNBinaryViewPtr ->
    Word64 ->
    CSize ->
    IO BNDataBufferPtr

foreign import ccall unsafe "BNFreeDataBuffer"
  c_BNFreeDataBuffer :: BNDataBufferPtr -> IO ()

foreign import ccall unsafe "BNGetDataBufferContents"
  c_BNGetDataBufferContents :: BNDataBufferPtr -> IO (Ptr CChar)

foreign import ccall unsafe "BNGetLowLevelILOwnerFunction"
  c_BNGetLowLevelILOwnderFunction :: BNLlilFunctionPtr -> IO BNFunctionPtr

foreign import ccall unsafe "BNLowLevelILGetInstructionStart"
  c_BNLowLevelILGetInstructionStart ::
    BNLlilFunctionPtr -> BNArchPtr -> Word64 -> IO CSize

foreign import ccall unsafe "BNGetLowLevelILInstructionCount"
  c_BNGetLowLevelILInstructionCount :: BNLlilFunctionPtr -> IO CSize

foreign import ccall unsafe "BNGetLowLevelILIndexForInstruction"
  c_BNGetLowLevelILIndexForInstruction ::
    BNLlilFunctionPtr -> Word64 -> IO CSize

foreign import ccall unsafe "BNGetLowLevelILByIndexPtr"
  c_BNGetLowLevelILByIndexPtr ::
    Ptr BNLowLevelILInstruction -> BNLlilFunctionPtr -> CSize -> IO (Ptr BNLowLevelILInstruction)

foreign import ccall "BNInitPlugins"
  c_BNInitPlugins :: CBool -> IO CBool

foreign import ccall "BNGetInstallDirectory"
  c_BNGetInstallDirectory :: IO CString

foreign import ccall "BNGetBundledPluginDirectory"
  c_BNGetBundledPluginDirectory :: IO CString

foreign import ccall "BNGetUserDirectory"
  c_BNGetUserDirectory :: IO CString

foreign import ccall "BNGetUserPluginDirectory"
  c_BNGetUserPluginDirectory :: IO CString

foreign import ccall "BNGetRepositoriesDirectory"
  c_BNGetRepositoriesDirectory :: IO CString

foreign import ccall unsafe "BNGetFunctionStart"
  c_BNGetFunctionStart :: BNFunctionPtr -> IO Word64

foreign import ccall unsafe "BNGetFunctionHighestAddress"
  c_BNGetFunctionHighestAddress :: BNFunctionPtr -> IO Word64

foreign import ccall unsafe "BNGetFunctionLowestAddress"
  c_BNGetFunctionLowestAddress :: BNFunctionPtr -> IO Word64

foreign import ccall unsafe "BNGetFunctionSymbol"
  c_BNGetFunctionSymbol :: BNFunctionPtr -> IO BNSymbolPtr

foreign import ccall unsafe "BNWasFunctionAutomaticallyDiscovered"
  c_BNWasFunctionAutomaticallyDiscovered :: BNFunctionPtr -> IO CBool

foreign import ccall unsafe "BNFunctionHasUserAnnotations"
  c_BNFunctionHasUserAnnotations :: BNFunctionPtr -> IO CBool

foreign import ccall unsafe "BNFunctionHasExplicitlyDefinedType"
  c_BNFunctionHasExplicitlyDefinedType :: BNFunctionPtr -> IO CBool

foreign import ccall unsafe "BNIsFunctionUpdateNeeded"
  c_BNIsFunctionUpdateNeeded :: BNFunctionPtr -> IO CBool

foreign import ccall unsafe "BNHasUnresolvedIndirectBranches"
  c_BNHasUnresolvedIndirectBranches :: BNFunctionPtr -> IO CBool

foreign import ccall "BNGetFunctionComment"
  c_BNGetFunctionComment :: BNFunctionPtr -> IO CString

foreign import ccall "BNSetFunctionComment"
  c_BNSetFunctionComment :: BNFunctionPtr -> CString -> IO ()

foreign import ccall "BNGetFunctionArchitecture"
  c_BNGetFunctionArchitecture :: BNFunctionPtr -> BNArchPtr

foreign import ccall unsafe "BNGetFunctionLowLevelIL"
  c_BNGetFunctionLowLevelIL :: BNFunctionPtr -> IO BNLlilFunctionPtr

foreign import ccall unsafe "BNGetFunctionMediumLevelIL"
  c_BNGetFunctionMediumLevelIL :: BNFunctionPtr -> IO BNMlilFunctionPtr

foreign import ccall unsafe "BNGetMediumLevelILSSAForm"
  c_BNGetMediumLevelILSSAForm :: BNMlilFunctionPtr -> IO BNMlilSSAFunctionPtr

foreign import ccall unsafe "BNGetMediumLevelILNonSSAForm"
  c_BNGetMediumLevelILNonSSAForm :: BNMlilSSAFunctionPtr -> IO BNMlilFunctionPtr

foreign import ccall unsafe "BNGetMediumLevelILOwnerFunction"
  c_BNGetMediumLevelILOwnerFunction :: BNMlilSSAFunctionPtr -> IO BNFunctionPtr

getProduct :: IO String
getProduct = do
  cStr <- c_BNGetProduct
  peekCString cStr

foreign import ccall "BNGetProductType"
  c_BNGetProductType :: IO CString

getProductType :: IO String
getProductType = do
  cStr <- c_BNGetProductType
  peekCString cStr

foreign import ccall "BNGetLicensedUserEmail"
  c_BNGetLicensedUserEmail :: IO CString

getLicensedUserEmail :: IO String
getLicensedUserEmail = do
  cStr <- c_BNGetLicensedUserEmail
  peekCString cStr

foreign import ccall "BNGetSerialNumber"
  c_BNGetSerialNumber :: IO CString

getSerialNumberString :: IO String
getSerialNumberString = do
  cStr <- c_BNGetSerialNumber
  peekCString cStr

foreign import ccall "BNGetVersionString"
  c_BNGetVersionString :: IO CString

getVersionString :: IO String
getVersionString = do
  cStr <- c_BNGetVersionString
  peekCString cStr

foreign import ccall "BNSetLicense"
  setLicense :: CString -> IO ()

foreign import ccall "BNShutdown"
  shutdown :: IO ()

foreign import ccall "BNGetUniqueIdentifierString"
  c_BNGetUniqueIdentifierString :: IO CString

getUniqueIdentifierString :: IO String
getUniqueIdentifierString = do
  cStr <- c_BNGetUniqueIdentifierString
  peekCString cStr

foreign import ccall "BNGetSettingsFileName"
  c_BNGetSettingsFileName :: IO CString

getSettingsFileName :: IO String
getSettingsFileName = do
  cStr <- c_BNGetSettingsFileName
  peekCString cStr

foreign import ccall "BNGetFileForView"
  c_BNGetFileForView :: BNBinaryViewPtr -> IO BNFileMetaDataPtr

foreign import ccall "BNCloseFile"
  closeFile :: BNFileMetaDataPtr -> IO ()

getFileForView :: BNBinaryViewPtr -> IO BNFileMetaDataPtr
getFileForView = c_BNGetFileForView

foreign import ccall "BNGetBasicBlockOutgoingEdges"
  c_BNGetBasicBlockOutgoingEdges :: BNBasicBlockPtr -> Ptr CULLong -> IO BNBasicBlockEdgePtr

foreign import ccall "BNGetBasicBlockIncomingEdges"
  c_BNGetBasicBlockIncomingEdges :: BNBasicBlockPtr -> Ptr CULLong -> IO BNBasicBlockEdgePtr

-- Returns unique instruction index for SSA Mlil function context
foreign import ccall "BNGetMediumLevelILSSAVarDefinition"
  c_BNGetMediumLevelILSSAVarDefinition :: BNMlilSSAFunctionPtr -> Ptr BNVariable -> CSize -> IO CSize

foreign import ccall "BNGetMediumLevelILSSAVarUses"
  c_BNGetMediumLevelILSSAVarUses :: BNMlilSSAFunctionPtr -> Ptr BNVariable -> CULLong -> Ptr CULLong -> IO (Ptr CSize)

-- BINARYNINJACOREAPI BNVariableNameAndType* BNGetFunctionVariables(BNFunction* func, size_t* count);
foreign import ccall "BNGetFunctionVariables"
  c_BNGetFunctionVariables :: BNMlilFunctionPtr -> Ptr CSize -> IO (Ptr BNVariableNameAndType)

-- 	BINARYNINJACOREAPI void BNFreeVariableNameAndTypeList(BNVariableNameAndType* vars, size_t count);
foreign import ccall "BNFreeVariableNameAndTypeList"
  c_BNFreeVariableNameAndTypeList :: Ptr BNVariableNameAndType -> CSize -> IO ()
