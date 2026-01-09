module Binja.Function
  ( Binja.Function.start,
    Binja.Function.highestAddress,
    Binja.Function.lowestAddress,
    Binja.Function.symbol,
    Binja.Function.auto,
    Binja.Function.architecture,
    Binja.Function.hasUserAnnotations,
    Binja.Function.hasExplicitlyDefinedType,
    Binja.Function.needsUpdate,
    Binja.Function.hasUnresolvedIndirectBranches,
    Binja.Function.getComment,
    Binja.Function.setComment,
    Binja.Function.ssaVars,
    Binja.Function.llil,
    Binja.Function.mlil,
    Binja.Function.mlilToSSA,
    Binja.Function.mlilSSA,
    Binja.Function.mlilToRawFunction,
    Binja.Function.print,
  )
where

import Binja.FFI
import Binja.Symbol
import Binja.Types (BNArchPtr, BNFunctionPtr, BNLlilFunctionPtr, BNMlilFunctionPtr, BNMlilSSAFunctionPtr, BNSSAVariable (..), BNVariable, CSize, Symbol, Word64, alloca, newCString, nullPtr, peek, peekArray, peekCString, poke, rawVar, version, when)
import Binja.Utils
import Control.Monad (unless)

start :: BNFunctionPtr -> IO Word64
start = c_BNGetFunctionStart

highestAddress :: BNFunctionPtr -> IO Word64
highestAddress = c_BNGetFunctionHighestAddress

lowestAddress :: BNFunctionPtr -> IO Word64
lowestAddress = c_BNGetFunctionLowestAddress

symbol :: BNFunctionPtr -> IO Symbol
symbol func = do
  p <- c_BNGetFunctionSymbol func
  if p == nullPtr
    then error "c_BNGetFunctionSymbol evaluated to null"
    else Binja.Symbol.create p

auto :: BNFunctionPtr -> IO Bool
auto = fmap Binja.Utils.toBool . c_BNWasFunctionAutomaticallyDiscovered

hasUserAnnotations :: BNFunctionPtr -> IO Bool
hasUserAnnotations = fmap Binja.Utils.toBool . c_BNFunctionHasUserAnnotations

hasExplicitlyDefinedType :: BNFunctionPtr -> IO Bool
hasExplicitlyDefinedType = fmap Binja.Utils.toBool . c_BNFunctionHasExplicitlyDefinedType

needsUpdate :: BNFunctionPtr -> IO Bool
needsUpdate = fmap Binja.Utils.toBool . c_BNIsFunctionUpdateNeeded

hasUnresolvedIndirectBranches :: BNFunctionPtr -> IO Bool
hasUnresolvedIndirectBranches = fmap Binja.Utils.toBool . c_BNHasUnresolvedIndirectBranches

getComment :: BNFunctionPtr -> IO String
getComment func = do
  cStr <- c_BNGetFunctionComment func
  peekCString cStr

architecture :: BNFunctionPtr -> BNArchPtr
architecture = c_BNGetFunctionArchitecture

setComment :: BNFunctionPtr -> String -> IO ()
setComment func comment = do
  cStr <- newCString comment
  c_BNSetFunctionComment func cStr

ssaVars :: BNMlilSSAFunctionPtr -> IO [BNSSAVariable]
ssaVars func = do
  alloca $ \countVarPtr -> do
    rawVarPtr <- c_BNGetMediumLevelILVariables func countVarPtr
    countVar <- fromIntegral <$> peek countVarPtr
    rawVarList <-
      if rawVarPtr == nullPtr || countVar == 0
        then pure []
        else peekArray countVar rawVarPtr
    alloca $ \countVersionPtr -> do
      rawVersionPtr <- c_BNGetMediumLevelILVariableSSAVersions func rawVarPtr countVersionPtr
      countVersion <- fromIntegral <$> peek countVersionPtr
      rawVersionList <-
        if rawVersionPtr == nullPtr
          then pure []
          else peekArray countVersion rawVersionPtr
      when (rawVarPtr /= nullPtr) $ c_BNFreeVariableList rawVarPtr
      when (rawVersionPtr /= nullPtr) $ c_BNFreeILInstructionList rawVersionPtr
      pure $ zipWith createSSAVar rawVarList rawVersionList
  where
    createSSAVar :: BNVariable -> CSize -> BNSSAVariable
    createSSAVar var ver =
      BNSSAVariable
        { rawVar = var,
          version = fromIntegral ver
        }

llil :: BNFunctionPtr -> IO BNLlilFunctionPtr
llil func = do
  if func == nullPtr
    then error "llil: func == nullPtr"
    else do
      llilFuncPtr <- c_BNGetFunctionLowLevelIL func
      if llilFuncPtr == nullPtr
        then error "llil: c_BNGetFunctionLowLevelIL evaluated to nullPtr"
        else pure llilFuncPtr

mlil :: BNFunctionPtr -> IO BNMlilFunctionPtr
mlil func = do
  if func == nullPtr
    then error "mlil: func == nullPtr"
    else do
      mlilFuncPtr <- c_BNGetFunctionMediumLevelIL func
      if mlilFuncPtr == nullPtr
        then do
          Binja.Function.print func
          error "mlil: c_BNGetFunctionMediumLevelIL evaluated to nullPtr"
        else pure mlilFuncPtr

mlilToSSA :: BNMlilFunctionPtr -> IO BNMlilSSAFunctionPtr
mlilToSSA func = do
  p <- c_BNGetMediumLevelILSSAForm func
  if p == nullPtr
    then error "mlilToSSA: c_BNGetMediumLevelILSSAForm evaluated to nullPtr"
    else pure p

mlilSSA :: BNFunctionPtr -> IO BNMlilSSAFunctionPtr
mlilSSA func = do
  mlilFunc <- mlil func
  c_BNGetMediumLevelILSSAForm mlilFunc

mlilToRawFunction :: BNMlilSSAFunctionPtr -> IO BNFunctionPtr
mlilToRawFunction func = do
  rawFunc <- c_BNGetMediumLevelILOwnerFunction func
  if rawFunc == nullPtr
    then error "mlilToRawFunction: BNGetMediumLevelILOwnerFunction evaluated to null"
    else pure rawFunc

print :: BNFunctionPtr -> IO ()
print func = do
  s <- start func
  hi <- highestAddress func
  lo <- lowestAddress func
  mSym <- symbol func
  isAuto <- Binja.Function.auto func
  userAnn <- hasUserAnnotations func
  explTy <- hasExplicitlyDefinedType func
  upd <- needsUpdate func
  indBr <- hasUnresolvedIndirectBranches func
  cmt <- getComment func
  putStrLn "== BNFunction =="
  putStrLn $ "  start: " ++ show s
  putStrLn $ "  lowestAddress: " ++ show lo
  putStrLn $ "  highestAddress: " ++ show hi
  putStrLn $ "  symbol: " ++ show mSym
  putStrLn $ "  automaticallyDiscovered: " ++ show isAuto
  putStrLn $ "  hasUserAnnotations: " ++ show userAnn
  putStrLn $ "  hasExplicitlyDefinedType: " ++ show explTy
  putStrLn $ "  needsUpdate: " ++ show upd
  putStrLn $ "  unresolvedIndirectBranches: " ++ show indBr
  unless (null cmt) $
    putStrLn $
      "  comment:  " ++ cmt
