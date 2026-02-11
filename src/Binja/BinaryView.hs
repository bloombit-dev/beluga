module Binja.BinaryView
  ( Binja.BinaryView.load,
    Binja.BinaryView.close,
    Binja.BinaryView.save,
    Binja.BinaryView.hasFunctions,
    Binja.BinaryView.hasSymbols,
    Binja.BinaryView.hasDataVariables,
    Binja.BinaryView.updateAnalysis,
    Binja.BinaryView.updateAnalysisAndWait,
    Binja.BinaryView.abortAnalysis,
    Binja.BinaryView.functions,
    Binja.BinaryView.entryFunctions,
    Binja.BinaryView.entryFunction,
    Binja.BinaryView.functionsContaining,
    Binja.BinaryView.functionsAt,
    Binja.BinaryView.functionsByName,
    Binja.BinaryView.symbols,
    Binja.BinaryView.symbolsByName,
    Binja.BinaryView.strings,
    Binja.BinaryView.read,
    Binja.BinaryView.symbolAt,
  )
where

import Binja.FFI
import Binja.Function
import Binja.Plugin
import Binja.Symbol
import Binja.Types
import Binja.Utils
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

-- It accepts a Haskell String for the filename and options and a Bool for updateAnalysis.
-- Here, we pass nullFunPtr and nullPtr for the progress callback and context.
loadFilename ::
  -- | Filename to load
  String ->
  -- | updateAnalysis flag
  Bool ->
  -- | Options string (JSON)
  String ->
  IO BNBinaryViewPtr
loadFilename filename updateAnalysisB options =
  withCString filename $ \cFilename ->
    withCString options $ \cOptions ->
      c_BNLoadFilename
        cFilename
        (if updateAnalysisB then CBool 1 else CBool 0)
        cOptions
        nullFunPtr -- no progress callback
        nullPtr -- no progress context

load :: String -> String -> IO BNBinaryViewPtr
load filename options = do
  _ <- initPlugins False
  viewPtr' <- loadFilename filename True options
  if viewPtr' == nullPtr
    then error $ "Failed to load binary view on file: " ++ filename
    else pure viewPtr'

close :: BNBinaryViewPtr -> IO ()
close view' = do
  fileMetaDataPtr <- getFileForView view'
  closeFile fileMetaDataPtr

hasFunctions :: BNBinaryViewPtr -> IO Bool
hasFunctions = fmap Binja.Utils.toBool . c_BNHasFunctions

hasSymbols :: BNBinaryViewPtr -> Bool
hasSymbols = Binja.Utils.toBool . c_BNHasSymbols

hasDataVariables :: BNBinaryViewPtr -> Bool
hasDataVariables = Binja.Utils.toBool . c_BNHasDataVariables

-- saves the original binary file to the (filename)
-- absolute filepath along with any modifications
save :: BNBinaryViewPtr -> String -> IO Bool
save view filename =
  withCString filename $ \cFilename -> do
    result <- c_BNSaveToFilename view cFilename
    pure (Binja.Utils.toBool result)

updateAnalysis :: BNBinaryViewPtr -> IO ()
updateAnalysis = c_BNUpdateAnalysis

-- updateAnalysisAndWait
-- starts the analysis process and blocks until it is complete. This method should be
-- used when it is necessary to ensure that analysis results are fully updated before
-- proceeding with further operations.
-- If an update is already in progress, this method chains a new update request to ensure that the update processes
-- all pending changes before the call was made.
updateAnalysisAndWait :: BNBinaryViewPtr -> IO ()
updateAnalysisAndWait = c_BNUpdateAnalysisAndWait

abortAnalysis :: BNBinaryViewPtr -> IO ()
abortAnalysis = c_BNAbortAnalysis

getFunctionList :: BNBinaryViewPtr -> IO FunctionList
getFunctionList view =
  alloca $ \countPtr -> do
    rawPtr <- c_BNGetAnalysisFunctionList view countPtr
    count' <- fromIntegral <$> peek countPtr
    xs <-
      if rawPtr == nullPtr || count' == 0
        then pure []
        else peekArray count' rawPtr
    arrPtr <- newForeignPtr rawPtr (c_BNFreeFunctionList rawPtr $ fromIntegral count')
    pure
      FunctionList
        { flArrayPtr = arrPtr,
          flCount = count',
          flList = xs,
          flViewPtr = view
        }

functions :: BNBinaryViewPtr -> IO [BNFunctionPtr]
functions = fmap flList . getFunctionList

getEntryFunctionList :: BNBinaryViewPtr -> IO FunctionList
getEntryFunctionList view =
  alloca $ \countPtr -> do
    rawPtr <- c_BNGetAllEntryFunctions view countPtr
    count' <- fromIntegral <$> peek countPtr
    xs <-
      if rawPtr == nullPtr || count' == 0
        then pure []
        else peekArray count' rawPtr
    arrPtr <- newForeignPtr rawPtr (c_BNFreeFunctionList rawPtr $ fromIntegral count')
    pure
      FunctionList
        { flArrayPtr = arrPtr,
          flCount = count',
          flList = xs,
          flViewPtr = view
        }

entryFunctions :: BNBinaryViewPtr -> IO [BNFunctionPtr]
entryFunctions = fmap flList . getEntryFunctionList

entryFunction :: BNBinaryViewPtr -> IO (Maybe BNFunctionPtr)
entryFunction view = do
  rawFunc <- c_BNGetAnalysisEntryPoint view
  if rawFunc == nullPtr
    then pure Nothing
    else pure $ Just rawFunc

functionsByName :: BNBinaryViewPtr -> String -> IO [BNFunctionPtr]
functionsByName view name' = do
  syms <- symbolsByName view name'
  let funcSyms = filter Binja.Symbol.isFunction syms
  xs <- mapM (functionsAt view . address) funcSyms
  pure $ concat xs

symbols :: BNBinaryViewPtr -> IO [Symbol]
symbols view =
  alloca $ \countPtr -> do
    rawPtr <- c_BNGetSymbols view countPtr nullPtr
    count' <- fromIntegral <$> peek countPtr
    xs <-
      if rawPtr == nullPtr || count' == 0
        then pure []
        else peekArray count' rawPtr
    _ <- newForeignPtr rawPtr (c_BNFreeSymbolList rawPtr $ fromIntegral count')
    mapM Binja.Symbol.create xs

symbolsByName :: BNBinaryViewPtr -> String -> IO [Symbol]
symbolsByName view name' = do
  syms <- Binja.BinaryView.symbols view
  pure $ filter (\s -> name s == name') syms

functionsContaining :: BNBinaryViewPtr -> Word64 -> IO [BNFunctionPtr]
functionsContaining view addr =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetAnalysisFunctionsContainingAddress view addr countPtr
    count' <- peek countPtr
    if arrPtr == nullPtr || count' == 0
      then pure []
      else do
        refs <- peekArray (fromIntegral count') (castPtr arrPtr :: Ptr BNFunctionPtr)
        c_BNFreeFunctionList arrPtr count'
        pure refs

functionsAt :: BNBinaryViewPtr -> Word64 -> IO [BNFunctionPtr]
functionsAt view addr =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetAnalysisFunctionsForAddress view addr countPtr
    count' <- peek countPtr
    if arrPtr == nullPtr || count' == 0
      then pure []
      else do
        refs <- peekArray (fromIntegral count') (castPtr arrPtr :: Ptr BNFunctionPtr)
        c_BNFreeFunctionList arrPtr count'
        pure refs

strings :: BNBinaryViewPtr -> IO [Maybe String]
strings view =
  alloca $ \countPtr -> do
    arrPtr <- c_BNGetStrings view countPtr
    count' <- fromIntegral <$> peek countPtr
    if arrPtr == nullPtr || count' == 0
      then pure []
      else do
        refs <- peekArray count' (castPtr arrPtr :: Ptr BNStringRef)
        c_BNFreeStringReferenceList arrPtr
        forM refs $ \(BNStringRef t s l) -> do
          mbs <- Binja.BinaryView.read view s l
          pure $ fmap (T.unpack . decodeByType t) mbs

decodeByType :: BNStringType -> BS.ByteString -> T.Text
decodeByType ty' = go
  where
    go = case ty' of
      AsciiString -> TE.decodeLatin1
      Utf8String -> TE.decodeUtf8With TEE.lenientDecode
      Utf16String -> decodeUtf16Auto
      Utf32String -> decodeUtf32Auto

    -- UTF-16: detect BOM; otherwise assume LE
    decodeUtf16Auto bs
      | hasPrefix [0xFF, 0xFE] bs = TE.decodeUtf16LEWith TEE.lenientDecode (BS.drop 2 bs)
      | hasPrefix [0xFE, 0xFF] bs = TE.decodeUtf16BEWith TEE.lenientDecode (BS.drop 2 bs)
      | otherwise = TE.decodeUtf16LEWith TEE.lenientDecode bs

    -- UTF-32: detect BOM; otherwise assume LE
    decodeUtf32Auto bs
      | hasPrefix [0xFF, 0xFE, 0x00, 0x00] bs = TE.decodeUtf32LEWith TEE.lenientDecode (BS.drop 4 bs)
      | hasPrefix [0x00, 0x00, 0xFE, 0xFF] bs = TE.decodeUtf32BEWith TEE.lenientDecode (BS.drop 4 bs)
      | otherwise = TE.decodeUtf32LEWith TEE.lenientDecode bs

    hasPrefix :: [Word8] -> BS.ByteString -> Bool
    hasPrefix pfx bs = BS.pack pfx `BS.isPrefixOf` bs

read :: BNBinaryViewPtr -> Word64 -> CSize -> IO (Maybe BS.ByteString)
read view addr len = do
  dataBuffer <- c_BNReadViewBuffer view addr len
  if dataBuffer == nullPtr
    then pure Nothing
    else do
      dataPtr <- c_BNGetDataBufferContents dataBuffer
      if dataPtr == nullPtr
        then pure Nothing
        else do
          bs <- BS.packCStringLen (dataPtr, fromIntegral len)
          c_BNFreeDataBuffer dataBuffer
          pure $ Just bs

symbolAt :: BNBinaryViewPtr -> Word64 -> IO (Maybe Binja.Types.Symbol)
symbolAt view addr = do
  symbolPtr <- c_BNGetSymbolByAddress view addr nullPtr
  if symbolPtr == nullPtr
    then do
      funcs <- functionsAt view addr
      if length funcs > 0
        then do
          sym <- Binja.Function.symbol $ head funcs
          pure $ Just sym
        else pure Nothing
    else do
      sym <- Binja.Symbol.create symbolPtr
      pure $ Just sym
