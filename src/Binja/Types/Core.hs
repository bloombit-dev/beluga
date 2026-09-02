{-# LANGUAGE DuplicateRecordFields #-}

module Binja.Types.Core
  ( CSize (..),
    CBool (..),
    CInt (..),
    CUInt (..),
    CULLong (..),
    Word8,
    Word32,
    Word64,
    Int64,
    CChar,
    Ptr,
    FunPtr,
    nullFunPtr,
    nullPtr,
    CString,
    withCString,
    newCString,
    peekCString,
    GHC.ForeignPtr.ForeignPtr,
    (.&.),
    castWord32ToFloat,
    castWord64ToDouble,
    float2Double,
    newForeignPtr,
    pointerSize,
    peek,
    peekElemOff,
    alloca,
    castPtr,
    poke,
    peekArray,
    forM,
    when,
    finally,
    BNBinaryView,
    BNBinaryViewPtr,
    BNFileMetadata,
    BNFileMetaDataPtr,
    BNProgressFunction,
    BNProgressFunctionPtr,
    BNFunctionPtr,
    BNSymbolPtr,
    BNNameSpace,
    BNNameSpacePtr,
    BNStringRef (..),
    BNStringRefPtr,
    BNStringType (..),
    BNVariableSourceType (..),
    BNDataBufferPtr,
    BNReferenceSourcePtr,
    Binja.Types.Arch.BNArchPtr,
    DataVariable (..),
    BNTypeWithConfidence (..),
    BNBoolWithConfidence (..),
    BNTypeClass (..),
    BNTypePtr,
    BNMlilFunctionPtr,
    BNMlilSSAFunctionPtr,
    BNLlilFunctionPtr,
    BNBasicBlockPtr,
    BasicBlockMlilSSA (..),
    BNBasicBlockEdgePtr,
    BNBasicBlockEdge (..),
    BasicBlockEdge (..),
    BNBranchType,
    BNValueRangePtr,
    BNLookupTableEntryPtr,
    BNLowLevelILInstruction (..),
    BNLowLevelILOperation (..),
    BNMediumLevelILInstruction (..),
    BNMediumLevelILOperation (..),
    BNPossibleValueSet (..),
    BNVariable (..),
    BNSSAVariable (..),
    BNParameterVariablesWithConfidence (..),
    Binja.Types.Arch.Architecture (..),
    ParameterVars (..),
    AnalysisContext (..),
    FunctionContext (..),
    SSAVariableContext (..),
    ILIntrinsic (..),
    TargetMap,
    Function (..),
    FunctionList (..),
    SymbolList (..),
    SymbolType (..),
    SymbolBinding (..),
    Symbol (..),
    BNRegisterValueType (..),
    BNReferenceSource (..),
    Binja.Types.Core.alignmentS,
    Binja.Types.Arch.getArch,
    Binja.Types.Arch.getIntrinsic,
    CoreMediumLevelILInstruction (..),
    MediumLevelILSSAInstruction (..),
    Localcall (..),
    Constant (..),
    Comparison (..),
    Arithmetic (..),
    Terminal (..),
    Syscall (..),
    Tailcall (..),
    ControlFlow (..),
    Return (..),
    Load (..),
    Store (..),
    Memory (..),
    Carry (..),
    VariableInstruction (..),
    SetVar (..),
    RegisterStack (..),
    IntrinsicInstruction (..),
    MediumLevelILCallSsaRec (..),
    MediumLevelILCallOutputSsaRec (..),
    MediumLevelILVarOutputSsaRec (..),
    MediumLevelILVarOutputSsaFieldRec (..),
    MediumLevelILVarOutputAliasedRec (..),
    MediumLevelILVarOutputAliasedFieldRec (..),
    MediumLevelILConstPtrRec (..),
    MediumLevelILNopRec (..),
    MediumLevelILRetRec (..),
    MediumLevelILVarSsaRec (..),
    MediumLevelILSetVarSsaRec (..),
    MediumLevelILJumpRec (..),
    MediumLevelILJumpToRec (..),
    MediumLevelILTailcallSsaRec (..),
    MediumLevelILImportRec (..),
    MediumLevelILAddressOfRec (..),
    MediumLevelILAddressOfFieldRec (..),
    MediumLevelILPassByRefRec (..),
    MediumLevelILReturnByRefRec (..),
    MediumLevelILLoadSsaRec (..),
    MediumLevelILConstRec (..),
    MediumLevelILIfRec (..),
    MediumLevelILCmpERec (..),
    MediumLevelILCmpNeRec (..),
    MediumLevelILCmpSleRec (..),
    MediumLevelILCmpSltRec (..),
    MediumLevelILCmpUltRec (..),
    MediumLevelILCmpUleRec (..),
    MediumLevelILCmpSgeRec (..),
    MediumLevelILCmpUgeRec (..),
    MediumLevelILCmpSgtRec (..),
    MediumLevelILCmpUgtRec (..),
    MediumLevelILAndRec (..),
    MediumLevelILOrRec (..),
    MediumLevelILXorRec (..),
    MediumLevelILLslRec (..),
    MediumLevelILLsrRec (..),
    MediumLevelILAsrRec (..),
    MediumLevelILRolRec (..),
    MediumLevelILRorRec (..),
    MediumLevelILMulRec (..),
    MediumLevelILAdcRec (..),
    MediumLevelILSbbRec (..),
    MediumLevelILRlcRec (..),
    MediumLevelILRrcRec (..),
    MediumLevelILNoretRec (..),
    MediumLevelILStoreSsaRec (..),
    MediumLevelILSetVarAliasedRec (..),
    MediumLevelILSetVarSsaFieldRec (..),
    MediumLevelILSetVarFieldRec (..),
    MediumLevelILVarSsaFieldRec (..),
    MediumLevelILGotoRec (..),
    MediumLevelILAddRec (..),
    MediumLevelILSubRec (..),
    MediumLevelILMuluDpRec (..),
    MediumLevelILMulsDpRec (..),
    MediumLevelILDivuRec (..),
    MediumLevelILDivuDpRec (..),
    MediumLevelILDivsRec (..),
    MediumLevelILDivsDpRec (..),
    MediumLevelILModuRec (..),
    MediumLevelILModuDpRec (..),
    MediumLevelILModsRec (..),
    MediumLevelILModsDpRec (..),
    MediumLevelILNegRec (..),
    MediumLevelILNotRec (..),
    MediumLevelILCeilRec (..),
    MediumLevelILSxRec (..),
    MediumLevelILZxRec (..),
    MediumLevelILLowPartRec (..),
    MediumLevelILFsqrtRec (..),
    MediumLevelILFnegRec (..),
    MediumLevelILFabsRec (..),
    MediumLevelILFloatToIntRec (..),
    MediumLevelILIntToFloatRec (..),
    MediumLevelILFloatConvRec (..),
    MediumLevelILRoundToIntRec (..),
    MediumLevelILFloorRec (..),
    MediumLevelILFtruncRec (..),
    MediumLevelILIntrinsicSsaRec (..),
    MediumLevelILIntrinsicRec (..),
    MediumLevelILBoolToIntRec (..),
    MediumLevelILVarAliasedRec (..),
    MediumLevelILVarAliasedFieldRec (..),
    MediumLevelILSetVarAliasedFieldRec (..),
    MediumLevelILLoadStructSsaRec (..),
    MediumLevelILStoreStructSsaRec (..),
    MediumLevelILFcmpERec (..),
    MediumLevelILFcmpNeRec (..),
    MediumLevelILFcmpLtRec (..),
    MediumLevelILFcmpLeRec (..),
    MediumLevelILFcmpGeRec (..),
    MediumLevelILFcmpGtRec (..),
    MediumLevelILFcmpORec (..),
    MediumLevelILFcmpUoRec (..),
    MediumLevelILFaddRec (..),
    MediumLevelILFsubRec (..),
    MediumLevelILFmulRec (..),
    MediumLevelILFdivRec (..),
    MediumLevelILConstDataRec (..),
    MediumLevelILAddOverflowRec (..),
    MediumLevelILFloatConstRec (..),
    MediumLevelILTestBitRec (..),
    MediumLevelILSetVarSplitRec (..),
    MediumLevelILCallParamSsaRec (..),
    MediumLevelILCallParamRec (..),
    MediumLevelILSetVarRec (..),
    MediumLevelILAssertRec (..),
    MediumLevelILForceVerRec (..),
    MediumLevelILLoadRec (..),
    MediumLevelILLoadStructRec (..),
    MediumLevelILStoreRec (..),
    MediumLevelILStoreStructRec (..),
    MediumLevelILVarRec (..),
    MediumLevelILVarFieldRec (..),
    MediumLevelILVarSplitRec (..),
    MediumLevelILExternPtrRec (..),
    MediumLevelILRetHintRec (..),
    MediumLevelILBpRec (..),
    MediumLevelILTrapRec (..),
    MediumLevelILUndefRec (..),
    MediumLevelILUnimplRec (..),
    MediumLevelILUnimplMemRec (..),
    MediumLevelILSetVarSplitSsaRec (..),
    MediumLevelILVarSplitSsaRec (..),
    MediumLevelILAssertSsaRec (..),
    MediumLevelILForceVerSsaRec (..),
    MediumLevelILCallUntypedSsaRec (..),
    MediumLevelILSeparateParamListRec (..),
    MediumLevelILSharedParamSlotRec (..),
    MediumLevelILVarOutputRec (..),
    MediumLevelILVarOutputFieldRec (..),
    MediumLevelILStoreOutputRec (..),
    MediumLevelILSyscallRec (..),
    MediumLevelILSyscallUntypedRec (..),
    MediumLevelILTailcallRec (..),
    MediumLevelILTailcallUntypedRec (..),
    MediumLevelILFreeVarSlotRec (..),
    MediumLevelILSyscallSsaRec (..),
    MediumLevelILSyscallUntypedSsaRec (..),
    MediumLevelILTailcallUntypedSsaRec (..),
    MediumLevelILMemoryIntrinsicOutputSsaRec (..),
    MediumLevelILMemoryIntrinsicSsaRec (..),
    MediumLevelILFreeVarSlotSsaRec (..),
    MediumLevelILVarPhiRec (..),
    MediumLevelILMemPhiRec (..),
    MediumLevelILBlockToExpandRec (..),
    MediumLevelILBswapRec (..),
    MediumLevelILPopcntRec (..),
    MediumLevelILClzRec (..),
    MediumLevelILCtzRec (..),
    MediumLevelILRbitRec (..),
    MediumLevelILClsRec (..),
    MediumLevelILMinsRec (..),
    MediumLevelILMaxsRec (..),
    MediumLevelILMinuRec (..),
    MediumLevelILMaxuRec (..),
    MediumLevelILAbsRec (..),
    CFGContext (..),
  )
where

import Binja.Types.Arch (Architecture, BNArchPtr, Intrinsic, getArch, getIntrinsic)
import Control.Exception (finally)
import Control.Monad (forM, when)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Map as Map
import Data.Set as Set
import Data.Word (Word32, Word64, Word8)
import Foreign
  ( Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    alloca,
    castPtr,
    peek,
    peekElemOff,
  )
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CChar, CInt (..), CSize (..), CUInt (..), CULLong (..))
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import GHC.Float (castWord32ToFloat, castWord64ToDouble, float2Double)
import GHC.ForeignPtr (ForeignPtr)
import Numeric (showHex)

-- | TODO: This is added here to get around module cycle between FFI.hs and Types.hs.
-- This will get moved back into FFI.hs after the Types.hs refactor.
foreign import ccall "BNGetTypeClass"
  c_BNGetTypeClass :: BNTypePtr -> IO Word8

foreign import ccall "BNGetTypeWidth"
  c_BNGetTypeWidth :: BNTypePtr -> IO Word64

foreign import ccall "BNGetTypeAlignment"
  c_BNGetTypeAlignment :: BNTypePtr -> IO CSize

foreign import ccall "BNIsTypeSignedPtr"
  c_BNIsTypeSignedPtr :: Ptr BNBoolWithConfidence -> BNTypePtr -> IO ()

foreign import ccall "BNIsTypeConstPtr"
  c_BNIsTypeConstPtr :: Ptr BNBoolWithConfidence -> BNTypePtr -> IO ()

foreign import ccall "BNIsTypeVolatilePtr"
  c_BNIsTypeVolatilePtr :: Ptr BNBoolWithConfidence -> BNTypePtr -> IO ()

foreign import ccall "BNGetChildTypePtr"
  c_BNGetChildTypePtr :: Ptr BNTypeWithConfidence -> BNTypePtr -> IO ()

pointerSize :: Int
pointerSize = sizeOf (undefined :: Ptr ())

alignmentS :: Int
alignmentS = 8

-------------------------
-- BinaryView Types
-------------------------

-- | Greatest common ancestor of all other binary ninja types
data BNBinaryView

type BNBinaryViewPtr = Ptr BNBinaryView

data BNFileMetadata

type BNFileMetaDataPtr = Ptr BNFileMetadata

-- | Type for the BNProgressFunction callback:
--   It is given a context pointer and two CSize values (e.g., current and total progress)
type BNProgressFunction = Ptr () -> CSize -> CSize -> IO CBool

type BNProgressFunctionPtr = FunPtr BNProgressFunction

data BNNameSpace

type BNNameSpacePtr = Ptr BNNameSpace

data BNFunction_

type BNFunctionPtr = Ptr BNFunction_

data BNSymbol_

type BNSymbolPtr = Ptr BNSymbol_

type BNStringRefPtr = Ptr BNStringRef

data BNDataBuffer_

type BNDataBufferPtr = Ptr BNDataBuffer_

data BNReferenceSource_

type BNReferenceSourcePtr = Ptr BNReferenceSource_

data BNMlilFunction_

type BNMlilFunctionPtr = Ptr BNMlilFunction_

data BNMlilSSAFunction_

type BNMlilSSAFunctionPtr = Ptr BNMlilSSAFunction_

data BNLlilFunction_

type BNLlilFunctionPtr = Ptr BNLlilFunction_

data BNValueRange_

type BNValueRangePtr = Ptr BNValueRange_

data BNLookupTableEntry_

type BNLookupTableEntryPtr = Ptr BNLookupTableEntry_

data BNBasicBlock_

type BNBasicBlockPtr = Ptr BNBasicBlock_

type BNBasicBlockEdgePtr = Ptr BNBasicBlockEdge

data BNType_

type BNTypePtr = Ptr BNType_

type TargetMap = [(CULLong, CULLong)]

-- | Note: Algebra.Graph.Labelled provided by Alga will be expanded on its next release
-- Making it a good candidate to use for the CFGContext graph type.
data CFGContext = CFGContext
  { graph :: Map.Map BasicBlockMlilSSA (Set.Set BasicBlockEdge),
    entry :: BasicBlockMlilSSA
  }
  deriving (Show)

-- | Central abstraction of Beluga
data AnalysisContext = AnalysisContext
  { -- | Binary View pointer which is the greatest common ancestor for all other types.
    viewHandle :: BNBinaryViewPtr,
    -- | Path to file used to derive AnalysisContext
    filename :: String,
    functions :: [FunctionContext],
    entryFunction :: Maybe FunctionContext,
    -- | List of entry functions like init_array, fini_array, TLS callbacks, etc.
    -- Exported functions in shared objects not included.
    entryFunctions :: [FunctionContext],
    symbols :: [Symbol],
    strings :: [String],
    dataVars :: [DataVariable]
    -- image base :: Word64
    -- sections :: [Section]
    -- segments :: [Segment]
  }

-- | Higher level abstraction of a medium level IL SSA variant function
data FunctionContext = FunctionContext
  { handle :: BNMlilSSAFunctionPtr,
    -- | The start address for the function
    start :: Word64,
    symbol :: Symbol,
    -- | True if the function was discovered via creation of a user function <https://api.binary.ninja/binaryninja.function-module.html#binaryninja.function.Function.auto python-doc>
    auto :: Bool,
    -- | Top level instructions
    instructions :: [MediumLevelILSSAInstruction],
    -- | Mapping from SSA variables to their definition site (if exists) and use sites
    ssaVars :: Map.Map BNSSAVariable SSAVariableContext,
    aliasedVars :: [BNVariable],
    parameterVars :: ParameterVars,
    architecture :: Binja.Types.Arch.Architecture,
    cfg :: CFGContext
  }
  deriving (Show)

-- | Higher level abstract of ssa variable defintion site (if exists) and use sites
data SSAVariableContext = SSAVariableContext
  { defSite :: Maybe MediumLevelILSSAInstruction,
    useSites :: [MediumLevelILSSAInstruction]
  }
  deriving (Show)

data BNVariable = BNVariable
  { varSourceType :: !BNVariableSourceType,
    varIndex :: !Word32,
    varStorage :: !Int64
  }
  deriving (Eq, Ord, Show)

instance Storable BNVariable where
  sizeOf _ = 16
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    t <- peekByteOff ptr 0 :: IO Word8
    r <- peekByteOff ptr 4 :: IO Word32
    s <- peekByteOff ptr 8 :: IO Int64
    pure (BNVariable (toEnum $ fromIntegral t) r s)
  poke ptr (BNVariable t r s) = do
    pokeByteOff ptr 0 $ (fromIntegral (fromEnum t) :: Word32)
    pokeByteOff ptr 4 r
    pokeByteOff ptr 8 s

data BNSSAVariable = BNSSAVariable
  { rawVar :: BNVariable,
    version :: Int
  }
  deriving (Eq, Ord, Show)

data BNParameterVariablesWithConfidence = BNParameterVariablesWithConfidence
  { pvVarPtr :: !(Ptr BNVariable),
    pvCount :: !CSize,
    pvConfidence :: !Word8
  }
  deriving (Show)

data ParameterVars = ParameterVars
  { vars :: [BNVariable],
    confidence :: Int
  }
  deriving (Show)

instance Storable BNParameterVariablesWithConfidence where
  sizeOf _ = 24
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    varPtr' <- peekByteOff ptr 0 :: IO (Ptr BNVariable)
    count' <- peekByteOff ptr 8 :: IO CSize
    confidence' <- peekByteOff ptr 16 :: IO Word8
    pure $ BNParameterVariablesWithConfidence varPtr' count' confidence'
  poke ptr (BNParameterVariablesWithConfidence varPtr' count' confidence') = do
    pokeByteOff ptr 0 varPtr'
    pokeByteOff ptr 8 count'
    pokeByteOff ptr 16 confidence'

data BNVariableSourceType
  = StackVariableSourceType
  | RegisterVariableSourceType
  | FlagVariableSourceType
  deriving (Eq, Ord, Show, Enum)

data BNTypeClass
  = VoidTypeClass
  | BoolTypeClass
  | IntegerTypeClass
  | FloatTypeClass
  | StructureTypeClass
  | EnumerationTypeClass
  | PointerTypeClass
  | ArrayTypeClass
  | FunctionTypeClass
  | VarArgsTypeClass
  | ValueTypeClass
  | NamedTypeReferenceClass
  | WideCharTypeClass
  | FragmentTypeClass
  deriving (Show, Eq, Enum)

data BNTypeWithConfidence = BNTypeWithConfidence
  { ty :: BNTypePtr,
    confidence :: Word8
  }
  deriving (Show)

instance Storable BNTypeWithConfidence where
  sizeOf _ = 16
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    value' <- peekByteOff ptr 0 :: IO BNTypePtr
    confidence' <- peekByteOff ptr 8
    pure
      BNTypeWithConfidence
        { ty = value',
          confidence = confidence'
        }
  poke ptr (BNTypeWithConfidence ty' confidence') = do
    pokeByteOff ptr 0 ty'
    pokeByteOff ptr 8 confidence'

data BNBoolWithConfidence = BNBoolWithConfidence
  { value :: Bool,
    confidence :: Word8
  }
  deriving (Show)

instance Storable BNBoolWithConfidence where
  sizeOf _ = 2
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    value' <- toBool <$> (peekByteOff ptr 0 :: IO CBool)
    confidence' <- peekByteOff ptr 1
    pure
      BNBoolWithConfidence
        { value = value',
          confidence = confidence'
        }
  poke ptr (BNBoolWithConfidence value' confidence') = do
    pokeByteOff ptr 0 (fromBool value' :: CBool)
    pokeByteOff ptr 1 confidence'

data DataVariable = DataVariable
  { address :: !Word64,
    ty :: !BNTypeClass,
    autoDiscovered :: !Bool,
    typeConfidence :: !Word8,
    width :: !Word64,
    alignment :: !CSize,
    isSigned :: BNBoolWithConfidence,
    isConst :: BNBoolWithConfidence,
    isVolatile :: BNBoolWithConfidence,
    bytes :: Maybe BS.ByteString
  }
  deriving (Show)

instance Storable DataVariable where
  sizeOf _ = 24
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    address' <- peekByteOff ptr 0
    ty' <- peekByteOff ptr 8 :: IO BNTypePtr
    tyClass' <- toEnum <$> fromIntegral <$> c_BNGetTypeClass ty' :: IO BNTypeClass
    autoDiscovered' <- toBool <$> (peekByteOff ptr 16 :: IO CBool)
    typeConfidence' <- peekByteOff ptr 17
    width' <- c_BNGetTypeWidth ty'
    alignment' <- c_BNGetTypeAlignment ty'
    isSigned' <- alloca $ \ptr' -> do
      c_BNIsTypeSignedPtr ptr' ty'
      peek ptr'
    isConst' <- alloca $ \ptr' -> do
      c_BNIsTypeConstPtr ptr' ty'
      peek ptr'
    isVolatile' <- alloca $ \ptr' -> do
      c_BNIsTypeVolatilePtr ptr' ty'
      peek ptr'
    -- if tyClass' == ArrayTypeClass
    --  then do
    --    alloca $ \ptr' -> do
    --      c_BNGetChildTypePtr ptr' ty'
    --      peeked <- peek ptr'
    --      let childTy' = (\BNTypeWithConfidence{ty=t} -> t) peeked
    --      liftedTyClass <- (toEnum <$> fromIntegral <$> c_BNGetTypeClass childTy') :: IO BNTypeClass
    --      _ <- Prelude.print $ "ArrayTypeClass with child type: " ++ (show (liftedTyClass :: BNTypeClass)) ++ " at 0x" ++ (showHex address' "")
    --      pure ()
    --  else do
    --    pure ()
    -- bytes' <- BinaryView.read address' width'
    pure
      DataVariable
        { address = address',
          ty = tyClass',
          autoDiscovered = autoDiscovered',
          typeConfidence = typeConfidence',
          width = width',
          alignment = alignment',
          isSigned = isSigned',
          isConst = isConst',
          isVolatile = isVolatile',
          bytes = Nothing
        }

  poke ptr (DataVariable address' ty' autoDiscovered' typeConfidence' _ _ _ _ _ _) = do
    pokeByteOff ptr 0 address'
    pokeByteOff ptr 8 (fromEnum ty')
    pokeByteOff ptr 16 (fromBool autoDiscovered' :: CBool)
    pokeByteOff ptr 17 typeConfidence'

data ILIntrinsic = ILIntrinsic
  { index :: !CSize,
    archHandle :: !BNArchPtr,
    arch :: Binja.Types.Arch.Architecture,
    intrinsic :: Binja.Types.Arch.Intrinsic
  }
  deriving (Show, Eq, Ord)

data BNBranchType
  = UnconditionalBranch
  | FalseBranch
  | TrueBranch
  | CallDestination
  | FunctionReturn
  | SystemCall
  | IndirectBranch
  | ExceptionBranch
  | UnresolvedBranch
  | UserDefinedBranch
  deriving (Show, Eq, Ord)

instance Enum BNBranchType where
  fromEnum UnconditionalBranch = 0
  fromEnum FalseBranch = 1
  fromEnum TrueBranch = 2
  fromEnum CallDestination = 3
  fromEnum FunctionReturn = 4
  fromEnum SystemCall = 5
  fromEnum IndirectBranch = 6
  fromEnum ExceptionBranch = 7
  fromEnum UnresolvedBranch = 127
  fromEnum UserDefinedBranch = 128

  toEnum 0 = UnconditionalBranch
  toEnum 1 = FalseBranch
  toEnum 2 = TrueBranch
  toEnum 3 = CallDestination
  toEnum 4 = FunctionReturn
  toEnum 5 = SystemCall
  toEnum 6 = IndirectBranch
  toEnum 7 = ExceptionBranch
  toEnum 127 = UnresolvedBranch
  toEnum 128 = UserDefinedBranch
  toEnum n = error $ "BNRegisterValueType.toEnum: invalid tag " ++ show n

data BNBasicBlockEdge = BNBasicBlockEdge
  { ty :: !BNBranchType,
    target :: !BNBasicBlockPtr,
    backEdge :: !CBool,
    fallThrough :: !CBool
  }
  deriving (Show, Eq, Ord)

data BasicBlockEdge = BasicBlockEdge
  { ty :: BNBranchType,
    target :: BasicBlockMlilSSA,
    backEdge :: Bool,
    -- | Whether this edge targets to a node whose control flow can eventually flow back through the source node of this edge.
    fallThrough :: Bool
  }
  deriving (Show, Eq, Ord)

data BasicBlockMlilSSA = BasicBlockMlilSSA
  { handle :: !BNBasicBlockPtr,
    start :: !CSize,
    instructions :: [MediumLevelILSSAInstruction],
    canExit :: !Bool,
    hasInvalidInstructions :: !Bool
  }
  deriving (Show, Eq, Ord)

instance Storable BNBasicBlockEdge where
  sizeOf _ = 24
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    ty' <- peekByteOff ptr 0 :: IO Word8
    target' <- peekByteOff ptr 8
    backEdge' <- peekByteOff ptr 16
    fallThrough' <- peekByteOff ptr 17
    pure $ BNBasicBlockEdge (toEnum $ fromIntegral ty') target' backEdge' fallThrough'
  poke ptr (BNBasicBlockEdge ty' target' backEdge' fallThrough') = do
    pokeByteOff ptr 0 $ fromEnum ty'
    pokeByteOff ptr 8 target'
    pokeByteOff ptr 16 backEdge'
    pokeByteOff ptr 17 fallThrough'

data BNPossibleValueSet = BNPossibleValueSet
  { pvsRegisterValTy :: !BNRegisterValueType,
    psvValue :: !Int64,
    pvsOffset :: !Int64,
    pvsSize :: !CSize,
    pvsRanges :: !BNValueRangePtr,
    pvsValueSet :: !(Ptr CInt),
    pvsLookupTbl :: !BNLookupTableEntryPtr,
    pvsCount :: !CSize
  }
  deriving (Show, Eq, Ord)

instance Storable BNPossibleValueSet where
  sizeOf _ = 64
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    rvt <- peekByteOff ptr 0 :: IO Word32
    val <- peekByteOff ptr 8
    offset' <- peekByteOff ptr 16
    size' <- peekByteOff ptr 24
    ranges <- peekByteOff ptr 32
    valueSet <- peekByteOff ptr 40
    lookupTbl <- peekByteOff ptr 48
    count' <- peekByteOff ptr 56
    pure (BNPossibleValueSet (toEnum $ fromIntegral rvt) val offset' size' ranges valueSet lookupTbl count')
  poke ptr (BNPossibleValueSet rvt val offset' size' ranges valueSet lookupTbl count') = do
    pokeByteOff ptr 0 $ fromEnum rvt
    pokeByteOff ptr 8 val
    pokeByteOff ptr 16 offset'
    pokeByteOff ptr 24 size'
    pokeByteOff ptr 32 ranges
    pokeByteOff ptr 40 valueSet
    pokeByteOff ptr 48 lookupTbl
    pokeByteOff ptr 56 count'

data Function = Function
  { funcAdvancedAnalysisRequests :: !Int,
    funcPtr :: !BNFunctionPtr,
    viewPtr :: !BNBinaryViewPtr
    -- , arch :: !BNArch
    -- , platform :: !BNPlatform
  }
  deriving (Eq, Show)

data BNStringRef = BNStringRef
  { bnType :: !BNStringType,
    bnStart :: !Word64,
    bnLength :: !CSize
  }
  deriving (Eq, Show)

instance Storable BNStringRef where
  sizeOf _ = 24
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    t <- peekByteOff ptr 0 :: IO Word8
    s <- peekByteOff ptr 8 :: IO Word64
    l <- peekByteOff ptr 16 :: IO CSize
    pure (BNStringRef (toEnum $ fromIntegral t) s l)
  poke ptr (BNStringRef t s l) = do
    pokeByteOff ptr 0 $ fromEnum t
    pokeByteOff ptr 8 s
    pokeByteOff ptr 16 l

data FunctionList = FunctionList
  { flArrayPtr :: !(ForeignPtr BNFunctionPtr),
    flCount :: !Int,
    flList :: ![BNFunctionPtr],
    flViewPtr :: !BNBinaryViewPtr
  }
  deriving (Eq, Show)

data SymbolList = SymbolList
  { slArrayPtr :: !(ForeignPtr BNSymbolPtr),
    slCount :: !Int,
    slList :: ![BNSymbolPtr],
    slViewPtr :: !BNBinaryViewPtr
  }
  deriving (Eq, Show)

data SymbolType
  = FunctionSymbol
  | ImportAddressSymbol
  | ImportedFunctionSymbol
  | DataSymbol
  | ImportedDataSymbol
  | ExternalSymbol
  | LibraryFunctionSymbol
  | SymbolicFunctionSymbol
  | LocalLabelSymbol
  deriving (Eq, Show, Enum, Ord)

data SymbolBinding = NoBinding | LocalBinding | GlobalBinding | WeakBinding
  deriving (Eq, Show, Enum, Ord)

data Symbol = Symbol
  { name :: String,
    ty :: SymbolType,
    binding :: SymbolBinding,
    address :: Word64,
    auto :: Bool
  }
  deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol name' ty' binding' address' auto') =
    "Symbol {name = "
      ++ show name'
      ++ ", ty = "
      ++ show ty'
      ++ ", binding = "
      ++ show binding'
      ++ ", address = 0x"
      ++ showHex address' ""
      ++ ", auto = "
      ++ show auto'
      ++ "}"

data BNStringType = AsciiString | Utf16String | Utf32String | Utf8String
  deriving (Eq, Show, Enum)

data BNLowLevelILInstruction = BNLowLevelILInstruction
  { llOperation :: !Word8,
    llAttributes :: !Word32,
    llSize :: !CSize,
    llFlags :: !CUInt,
    llSourceOperand :: !CUInt,
    llOp0 :: !CULLong,
    llOp1 :: !CULLong,
    llOp2 :: !CULLong,
    llOp3 :: !CULLong,
    llAddress :: !CULLong
  }
  deriving (Eq, Show)

instance Storable BNLowLevelILInstruction where
  sizeOf _ = 64
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    op <- peekByteOff ptr 0 :: IO Word8
    attr <- peekByteOff ptr 4
    sz <- peekByteOff ptr 8
    flg <- peekByteOff ptr 16
    srcOp <- peekByteOff ptr 20
    o0 <- peekByteOff ptr 24
    o1 <- peekByteOff ptr 32
    o2 <- peekByteOff ptr 40
    o3 <- peekByteOff ptr 48
    addr <- peekByteOff ptr 56
    pure (BNLowLevelILInstruction op attr sz flg srcOp o0 o1 o2 o3 addr)
  poke ptr (BNLowLevelILInstruction op attr sz flg srcOp o0 o1 o2 o3 addr) = do
    pokeByteOff ptr 0 op
    pokeByteOff ptr 4 attr
    pokeByteOff ptr 8 sz
    pokeByteOff ptr 16 flg
    pokeByteOff ptr 20 srcOp
    pokeByteOff ptr 24 o0
    pokeByteOff ptr 32 o1
    pokeByteOff ptr 40 o2
    pokeByteOff ptr 48 o3
    pokeByteOff ptr 56 addr

data BNLowLevelILOperation
  = LLIL_NOP
  | LLIL_SET_REG
  | LLIL_SET_REG_SPLIT
  | LLIL_SET_FLAG
  | LLIL_SET_REG_STACK_REL
  | LLIL_REG_STACK_PUSH
  | LLIL_ASSERT
  | LLIL_FORCE_VER
  | LLIL_LOAD
  | LLIL_STORE
  | LLIL_PUSH
  | LLIL_POP
  | LLIL_REG
  | LLIL_REG_SPLIT
  | LLIL_REG_STACK_REL
  | LLIL_REG_STACK_POP
  | LLIL_REG_STACK_FREE_REG
  | LLIL_REG_STACK_FREE_REL
  | LLIL_CONST
  | LLIL_CONST_PTR
  | LLIL_EXTERN_PTR
  | LLIL_FLOAT_CONST
  | LLIL_FLAG
  | LLIL_FLAG_BIT
  | LLIL_ADD
  | LLIL_ADC
  | LLIL_SUB
  | LLIL_SBB
  | LLIL_AND
  | LLIL_OR
  | LLIL_XOR
  | LLIL_LSL
  | LLIL_LSR
  | LLIL_ASR
  | LLIL_ROL
  | LLIL_RLC
  | LLIL_ROR
  | LLIL_RRC
  | LLIL_MUL
  | LLIL_MULU_DP
  | LLIL_MULS_DP
  | LLIL_DIVU
  | LLIL_DIVU_DP
  | LLIL_DIVS
  | LLIL_DIVS_DP
  | LLIL_MODU
  | LLIL_MODU_DP
  | LLIL_MODS
  | LLIL_MODS_DP
  | LLIL_NEG
  | LLIL_NOT
  | LLIL_SX
  | LLIL_ZX
  | LLIL_LOW_PART
  | LLIL_JUMP
  | LLIL_JUMP_TO
  | LLIL_CALL
  | LLIL_CALL_STACK_ADJUST
  | LLIL_TAILCALL
  | LLIL_RET
  | LLIL_NORET
  | LLIL_IF
  | LLIL_GOTO
  | LLIL_FLAG_COND
  | LLIL_FLAG_GROUP
  | LLIL_CMP_E
  | LLIL_CMP_NE
  | LLIL_CMP_SLT
  | LLIL_CMP_ULT
  | LLIL_CMP_SLE
  | LLIL_CMP_ULE
  | LLIL_CMP_SGE
  | LLIL_CMP_UGE
  | LLIL_CMP_SGT
  | LLIL_CMP_UGT
  | LLIL_TEST_BIT
  | LLIL_BOOL_TO_INT
  | LLIL_ADD_OVERFLOW
  | LLIL_SYSCALL
  | LLIL_BP
  | LLIL_TRAP
  | LLIL_INTRINSIC
  | LLIL_UNDEF
  | LLIL_UNIMPL
  | LLIL_UNIMPL_MEM
  | LLIL_FADD
  | LLIL_FSUB
  | LLIL_FMUL
  | LLIL_FDIV
  | LLIL_FSQRT
  | LLIL_FNEG
  | LLIL_FABS
  | LLIL_FLOAT_TO_INT
  | LLIL_INT_TO_FLOAT
  | LLIL_FLOAT_CONV
  | LLIL_ROUND_TO_INT
  | LLIL_FLOOR
  | LLIL_CEIL
  | LLIL_FTRUNC
  | LLIL_FCMP_E
  | LLIL_FCMP_NE
  | LLIL_FCMP_LT
  | LLIL_FCMP_LE
  | LLIL_FCMP_GE
  | LLIL_FCMP_GT
  | LLIL_FCMP_O
  | LLIL_FCMP_UO
  | LLIL_SET_REG_SSA
  | LLIL_SET_REG_SSA_PARTIAL
  | LLIL_SET_REG_SPLIT_SSA
  | LLIL_SET_REG_STACK_REL_SSA
  | LLIL_SET_REG_STACK_ABS_SSA
  | LLIL_REG_SPLIT_DEST_SSA
  | LLIL_REG_STACK_DEST_SSA
  | LLIL_REG_SSA
  | LLIL_REG_SSA_PARTIAL
  | LLIL_REG_SPLIT_SSA
  | LLIL_REG_STACK_REL_SSA
  | LLIL_REG_STACK_ABS_SSA
  | LLIL_REG_STACK_FREE_REL_SSA
  | LLIL_REG_STACK_FREE_ABS_SSA
  | LLIL_SET_FLAG_SSA
  | LLIL_ASSERT_SSA
  | LLIL_FORCE_VER_SSA
  | LLIL_FLAG_SSA
  | LLIL_FLAG_BIT_SSA
  | LLIL_CALL_SSA
  | LLIL_SYSCALL_SSA
  | LLIL_TAILCALL_SSA
  | LLIL_CALL_PARAM
  | LLIL_CALL_STACK_SSA
  | LLIL_CALL_OUTPUT_SSA
  | LLIL_SEPARATE_PARAM_LIST_SSA
  | LLIL_SHARED_PARAM_SLOT_SSA
  | LLIL_MEMORY_INTRINSIC_OUTPUT_SSA
  | LLIL_LOAD_SSA
  | LLIL_STORE_SSA
  | LLIL_INTRINSIC_SSA
  | LLIL_MEMORY_INTRINSIC_SSA
  | LLIL_REG_PHI
  | LLIL_REG_STACK_PHI
  | LLIL_FLAG_PHI
  | LLIL_MEM_PHI
  | LLIL_BSWAP
  | LLIL_POPCNT
  | LLIL_CLZ
  | LLIL_CTZ
  | LLIL_RBIT
  | LLIL_CLS
  | LLIL_MINS
  | LLIL_MAXS
  | LLIL_MINU
  | LLIL_MAXU
  | LLIL_ABS
  deriving (Eq, Show, Enum)

data BNRegisterValueType
  = UndeterminedValue
  | EntryValue
  | ConstantValue
  | ConstantPointerValue
  | ExternalPointerValue
  | StackFrameOffset
  | ReturnAddressValue
  | ImportedAddressValue
  | SignedRangeValue
  | UnsignedRangeValue
  | LookupTableValue
  | InSetOfValues
  | NotInSetOfValues
  | ConstantDataValue
  | ConstantDataZeroExtendValue
  | ConstantDataSignExtendValue
  | ConstantDataAggregateValue
  deriving (Eq, Ord, Show)

instance Enum BNRegisterValueType where
  fromEnum UndeterminedValue = 0
  fromEnum EntryValue = 1
  fromEnum ConstantValue = 2
  fromEnum ConstantPointerValue = 3
  fromEnum ExternalPointerValue = 4
  fromEnum StackFrameOffset = 5
  fromEnum ReturnAddressValue = 6
  fromEnum ImportedAddressValue = 7
  fromEnum SignedRangeValue = 8
  fromEnum UnsignedRangeValue = 9
  fromEnum LookupTableValue = 10
  fromEnum InSetOfValues = 11
  fromEnum NotInSetOfValues = 12
  fromEnum ConstantDataValue = 0x8000
  fromEnum ConstantDataZeroExtendValue = 0x8001
  fromEnum ConstantDataSignExtendValue = 0x8002
  fromEnum ConstantDataAggregateValue = 0x8003

  toEnum 0x0000 = UndeterminedValue
  toEnum 0x0001 = EntryValue
  toEnum 0x0002 = ConstantValue
  toEnum 0x0003 = ConstantPointerValue
  toEnum 0x0004 = ExternalPointerValue
  toEnum 0x0005 = StackFrameOffset
  toEnum 0x0006 = ReturnAddressValue
  toEnum 0x0007 = ImportedAddressValue
  toEnum 0x0008 = SignedRangeValue
  toEnum 0x0009 = UnsignedRangeValue
  toEnum 0x000A = LookupTableValue
  toEnum 0x000B = InSetOfValues
  toEnum 0x000C = NotInSetOfValues
  toEnum 0x8000 = ConstantDataValue
  toEnum 0x8001 = ConstantDataZeroExtendValue
  toEnum 0x8002 = ConstantDataSignExtendValue
  toEnum 0x8003 = ConstantDataAggregateValue
  toEnum n = error $ "BNRegisterValueType.toEnum: invalid tag " ++ show n

instance Storable BNReferenceSource where
  sizeOf _ = 24
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    f <- peekByteOff ptr 0 :: IO BNFunctionPtr
    a <- peekByteOff ptr 8 :: IO BNArchPtr
    addr <- peekByteOff ptr 16 :: IO Word64
    pure $ BNReferenceSource f a addr
  poke ptr (BNReferenceSource f a addr) = do
    pokeByteOff ptr 0 f
    pokeByteOff ptr 8 a
    pokeByteOff ptr 16 addr

data BNReferenceSource = BNReferenceSource
  { bnFunc :: !BNFunctionPtr,
    bnArch :: !BNArchPtr,
    bnAddr :: !Word64
  }
  deriving (Show, Eq)

data BNMediumLevelILOperation
  = MLIL_NOP
  | MLIL_SET_VAR
  | MLIL_SET_VAR_FIELD
  | MLIL_SET_VAR_SPLIT
  | MLIL_ASSERT
  | MLIL_FORCE_VER
  | MLIL_LOAD
  | MLIL_LOAD_STRUCT
  | MLIL_STORE
  | MLIL_STORE_STRUCT
  | MLIL_VAR
  | MLIL_VAR_FIELD
  | MLIL_VAR_SPLIT
  | MLIL_ADDRESS_OF
  | MLIL_ADDRESS_OF_FIELD
  | MLIL_PASS_BY_REF
  | MLIL_RETURN_BY_REF
  | MLIL_CONST
  | MLIL_CONST_DATA
  | MLIL_CONST_PTR
  | MLIL_EXTERN_PTR
  | MLIL_FLOAT_CONST
  | MLIL_IMPORT
  | MLIL_ADD
  | MLIL_ADC
  | MLIL_SUB
  | MLIL_SBB
  | MLIL_AND
  | MLIL_OR
  | MLIL_XOR
  | MLIL_LSL
  | MLIL_LSR
  | MLIL_ASR
  | MLIL_ROL
  | MLIL_RLC
  | MLIL_ROR
  | MLIL_RRC
  | MLIL_MUL
  | MLIL_MULU_DP
  | MLIL_MULS_DP
  | MLIL_DIVU
  | MLIL_DIVU_DP
  | MLIL_DIVS
  | MLIL_DIVS_DP
  | MLIL_MODU
  | MLIL_MODU_DP
  | MLIL_MODS
  | MLIL_MODS_DP
  | MLIL_NEG
  | MLIL_NOT
  | MLIL_SX
  | MLIL_ZX
  | MLIL_LOW_PART
  | MLIL_JUMP
  | MLIL_JUMP_TO
  | MLIL_RET_HINT
  | MLIL_CALL
  | MLIL_CALL_UNTYPED
  | MLIL_CALL_PARAM
  | MLIL_SEPARATE_PARAM_LIST
  | MLIL_SHARED_PARAM_SLOT
  | MLIL_VAR_OUTPUT
  | MLIL_VAR_OUTPUT_FIELD
  | MLIL_STORE_OUTPUT
  | MLIL_RET
  | MLIL_NORET
  | MLIL_IF
  | MLIL_GOTO
  | MLIL_CMP_E
  | MLIL_CMP_NE
  | MLIL_CMP_SLT
  | MLIL_CMP_ULT
  | MLIL_CMP_SLE
  | MLIL_CMP_ULE
  | MLIL_CMP_SGE
  | MLIL_CMP_UGE
  | MLIL_CMP_SGT
  | MLIL_CMP_UGT
  | MLIL_TEST_BIT
  | MLIL_BOOL_TO_INT
  | MLIL_ADD_OVERFLOW
  | MLIL_SYSCALL
  | MLIL_SYSCALL_UNTYPED
  | MLIL_TAILCALL
  | MLIL_TAILCALL_UNTYPED
  | MLIL_INTRINSIC
  | MLIL_FREE_VAR_SLOT
  | MLIL_BP
  | MLIL_TRAP
  | MLIL_UNDEF
  | MLIL_UNIMPL
  | MLIL_UNIMPL_MEM
  | MLIL_FADD
  | MLIL_FSUB
  | MLIL_FMUL
  | MLIL_FDIV
  | MLIL_FSQRT
  | MLIL_FNEG
  | MLIL_FABS
  | MLIL_FLOAT_TO_INT
  | MLIL_INT_TO_FLOAT
  | MLIL_FLOAT_CONV
  | MLIL_ROUND_TO_INT
  | MLIL_FLOOR
  | MLIL_CEIL
  | MLIL_FTRUNC
  | MLIL_FCMP_E
  | MLIL_FCMP_NE
  | MLIL_FCMP_LT
  | MLIL_FCMP_LE
  | MLIL_FCMP_GE
  | MLIL_FCMP_GT
  | MLIL_FCMP_O
  | MLIL_FCMP_UO
  | MLIL_SET_VAR_SSA
  | MLIL_SET_VAR_SSA_FIELD
  | MLIL_SET_VAR_SPLIT_SSA
  | MLIL_SET_VAR_ALIASED
  | MLIL_SET_VAR_ALIASED_FIELD
  | MLIL_VAR_SSA
  | MLIL_VAR_SSA_FIELD
  | MLIL_VAR_ALIASED
  | MLIL_VAR_ALIASED_FIELD
  | MLIL_VAR_SPLIT_SSA
  | MLIL_ASSERT_SSA
  | MLIL_FORCE_VER_SSA
  | MLIL_CALL_SSA
  | MLIL_CALL_UNTYPED_SSA
  | MLIL_SYSCALL_SSA
  | MLIL_SYSCALL_UNTYPED_SSA
  | MLIL_TAILCALL_SSA
  | MLIL_TAILCALL_UNTYPED_SSA
  | MLIL_CALL_PARAM_SSA
  | MLIL_CALL_OUTPUT_SSA
  | MLIL_VAR_OUTPUT_SSA
  | MLIL_VAR_OUTPUT_SSA_FIELD
  | MLIL_VAR_OUTPUT_ALIASED
  | MLIL_VAR_OUTPUT_ALIASED_FIELD
  | MLIL_MEMORY_INTRINSIC_OUTPUT_SSA
  | MLIL_LOAD_SSA
  | MLIL_LOAD_STRUCT_SSA
  | MLIL_STORE_SSA
  | MLIL_STORE_STRUCT_SSA
  | MLIL_INTRINSIC_SSA
  | MLIL_MEMORY_INTRINSIC_SSA
  | MLIL_FREE_VAR_SLOT_SSA
  | MLIL_VAR_PHI
  | MLIL_MEM_PHI
  | MLIL_BLOCK_TO_EXPAND
  | MLIL_BSWAP
  | MLIL_POPCNT
  | MLIL_CLZ
  | MLIL_CTZ
  | MLIL_RBIT
  | MLIL_CLS
  | MLIL_MINS
  | MLIL_MAXS
  | MLIL_MINU
  | MLIL_MAXU
  | MLIL_ABS
  deriving (Eq, Ord, Show, Enum)

data BNMediumLevelILInstruction = BNMediumLevelILInstruction
  { mlOperation :: !BNMediumLevelILOperation,
    mlAttributes :: !Word32,
    mlSourceOperand :: !Word32,
    mlSize :: !CSize,
    mlOp0 :: !Word64,
    mlOp1 :: !Word64,
    mlOp2 :: !Word64,
    mlOp3 :: !Word64,
    mlOp4 :: !Word64,
    mlAddress :: !Word64
  }
  deriving (Eq, Ord, Show)

instance Storable BNMediumLevelILInstruction where
  sizeOf _ = 72
  alignment _ = Binja.Types.Core.alignmentS
  peek ptr = do
    op <- peekByteOff ptr 0 :: IO Word8
    attr <- peekByteOff ptr 4 :: IO Word32
    srcOp <- peekByteOff ptr 8 :: IO Word32
    sz <- peekByteOff ptr 16 :: IO CSize
    o0 <- peekByteOff ptr 24 :: IO Word64
    o1 <- peekByteOff ptr 32 :: IO Word64
    o2 <- peekByteOff ptr 40 :: IO Word64
    o3 <- peekByteOff ptr 48 :: IO Word64
    o4 <- peekByteOff ptr 56 :: IO Word64
    addr <- peekByteOff ptr 64 :: IO Word64
    pure
      ( BNMediumLevelILInstruction
          (toEnum $ fromIntegral op)
          attr
          srcOp
          sz
          o0
          o1
          o2
          o3
          o4
          addr
      )
  poke ptr (BNMediumLevelILInstruction op attr srcOp sz o0 o1 o2 o3 o4 addr) = do
    pokeByteOff ptr 0 $ fromEnum op
    pokeByteOff ptr 4 attr
    pokeByteOff ptr 8 srcOp
    pokeByteOff ptr 16 sz
    pokeByteOff ptr 24 o0
    pokeByteOff ptr 32 o1
    pokeByteOff ptr 40 o2
    pokeByteOff ptr 48 o3
    pokeByteOff ptr 56 o4
    pokeByteOff ptr 64 addr

data CoreMediumLevelILInstruction = CoreMediumLevelILInstruction
  { instr :: BNMediumLevelILInstruction,
    ilFunc :: BNMlilSSAFunctionPtr,
    exprIndex :: CSize
  }
  deriving (Show, Eq, Ord)

data MediumLevelILNopRec = MediumLevelILNopRec
  {core :: CoreMediumLevelILInstruction}
  deriving (Show, Eq, Ord)

data MediumLevelILCallSsaRec = MediumLevelILCallSsaRec
  { output :: [BNSSAVariable],
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    srcMemory :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCallOutputSsaRec = MediumLevelILCallOutputSsaRec
  { destMemory :: Int,
    dest :: [BNSSAVariable],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarOutputSsaRec = MediumLevelILVarOutputSsaRec
  { dest :: BNSSAVariable,
    var :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarOutputSsaFieldRec = MediumLevelILVarOutputSsaFieldRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarOutputAliasedRec = MediumLevelILVarOutputAliasedRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarOutputAliasedFieldRec = MediumLevelILVarOutputAliasedFieldRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCallParamSsaRec = MediumLevelILCallParamSsaRec
  { srcMemory :: Int,
    src :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCallParamRec = MediumLevelILCallParamRec
  { src :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILConstPtrRec = MediumLevelILConstPtrRec
  { constant :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILConstRec = MediumLevelILConstRec
  { constant :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRetRec = MediumLevelILRetRec
  { src :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarSsaRec = MediumLevelILVarSsaRec
  { src :: BNSSAVariable,
    var :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarSsaRec = MediumLevelILSetVarSsaRec
  { dest :: BNSSAVariable,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILJumpRec = MediumLevelILJumpRec
  { dest :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILTailcallSsaRec = MediumLevelILTailcallSsaRec
  { output :: [BNSSAVariable],
    outputDestMemory :: Int,
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    srcMemory :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILImportRec = MediumLevelILImportRec
  { constant :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAddressOfRec = MediumLevelILAddressOfRec
  { src :: BNVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAddressOfFieldRec = MediumLevelILAddressOfFieldRec
  { src :: BNVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILPassByRefRec = MediumLevelILPassByRefRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILReturnByRefRec = MediumLevelILReturnByRefRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLoadSsaRec = MediumLevelILLoadSsaRec
  { src :: MediumLevelILSSAInstruction,
    srcMemory :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILIfRec = MediumLevelILIfRec
  { condition :: MediumLevelILSSAInstruction,
    true :: Int, -- Instruction Index
    false :: Int, -- Instruction Index
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpERec = MediumLevelILCmpERec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpSleRec = MediumLevelILCmpSleRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpNeRec = MediumLevelILCmpNeRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpSltRec = MediumLevelILCmpSltRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpUltRec = MediumLevelILCmpUltRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpUleRec = MediumLevelILCmpUleRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpSgeRec = MediumLevelILCmpSgeRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpUgeRec = MediumLevelILCmpUgeRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpSgtRec = MediumLevelILCmpSgtRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCmpUgtRec = MediumLevelILCmpUgtRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAndRec = MediumLevelILAndRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILOrRec = MediumLevelILOrRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILXorRec = MediumLevelILXorRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLslRec = MediumLevelILLslRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLsrRec = MediumLevelILLsrRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAsrRec = MediumLevelILAsrRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRolRec = MediumLevelILRolRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRorRec = MediumLevelILRorRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMulRec = MediumLevelILMulRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMuluRec = MediumLevelILMuluRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMuluDpRec = MediumLevelILMuluDpRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMulsDpRec = MediumLevelILMulsDpRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILDivsRec = MediumLevelILDivsRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILDivuRec = MediumLevelILDivuRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILDivuDpRec = MediumLevelILDivuDpRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILDivsDpRec = MediumLevelILDivsDpRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILModuRec = MediumLevelILModuRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILModuDpRec = MediumLevelILModuDpRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILModsRec = MediumLevelILModsRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILModsDpRec = MediumLevelILModsDpRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAddOverflowRec = MediumLevelILAddOverflowRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILNoretRec = MediumLevelILNoretRec
  { core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILStoreSsaRec = MediumLevelILStoreSsaRec
  { dest :: MediumLevelILSSAInstruction,
    destMemory :: Int,
    srcMemory :: Int,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarSsaFieldRec = MediumLevelILSetVarSsaFieldRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    offset :: Int,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarAliasedRec = MediumLevelILSetVarAliasedRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarSsaFieldRec = MediumLevelILVarSsaFieldRec
  { src :: BNSSAVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILGotoRec = MediumLevelILGotoRec
  { dest :: Int, -- InstructionIndex
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAddRec = MediumLevelILAddRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSubRec = MediumLevelILSubRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILNegRec = MediumLevelILNegRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILNotRec = MediumLevelILNotRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSxRec = MediumLevelILSxRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILZxRec = MediumLevelILZxRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLowPartRec = MediumLevelILLowPartRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFsqrtRec = MediumLevelILFsqrtRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFnegRec = MediumLevelILFnegRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFabsRec = MediumLevelILFabsRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFloatToIntRec = MediumLevelILFloatToIntRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILIntToFloatRec = MediumLevelILIntToFloatRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFloatConvRec = MediumLevelILFloatConvRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRoundToIntRec = MediumLevelILRoundToIntRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFloorRec = MediumLevelILFloorRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCeilRec = MediumLevelILCeilRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFtruncRec = MediumLevelILFtruncRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILJumpToRec = MediumLevelILJumpToRec
  { dest :: MediumLevelILSSAInstruction,
    target :: TargetMap,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILIntrinsicSsaRec = MediumLevelILIntrinsicSsaRec
  { output :: [BNSSAVariable],
    intrinsic :: ILIntrinsic,
    params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILIntrinsicRec = MediumLevelILIntrinsicRec
  { output :: [BNVariable],
    intrinsic :: ILIntrinsic,
    params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILBoolToIntRec = MediumLevelILBoolToIntRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarAliasedRec = MediumLevelILVarAliasedRec
  { src :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarAliasedFieldRec = MediumLevelILVarAliasedFieldRec
  { src :: BNSSAVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarAliasedFieldRec = MediumLevelILSetVarAliasedFieldRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    offset :: Int,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLoadStructSsaRec = MediumLevelILLoadStructSsaRec
  { src :: MediumLevelILSSAInstruction,
    offset :: Int,
    srcMemory :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILStoreStructSsaRec = MediumLevelILStoreStructSsaRec
  { dest :: MediumLevelILSSAInstruction,
    offset :: Int,
    destMemory :: Int,
    srcMemory :: Int,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpERec = MediumLevelILFcmpERec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpNeRec = MediumLevelILFcmpNeRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpLtRec = MediumLevelILFcmpLtRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpLeRec = MediumLevelILFcmpLeRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpGeRec = MediumLevelILFcmpGeRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpGtRec = MediumLevelILFcmpGtRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpORec = MediumLevelILFcmpORec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFcmpUoRec = MediumLevelILFcmpUoRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFmulRec = MediumLevelILFmulRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFdivRec = MediumLevelILFdivRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFsubRec = MediumLevelILFsubRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFaddRec = MediumLevelILFaddRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILTestBitRec = MediumLevelILTestBitRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILConstDataRec = MediumLevelILConstDataRec
  { constant :: BNDataBufferPtr,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFloatConstRec = MediumLevelILFloatConstRec
  { constant :: Double,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAdcRec = MediumLevelILAdcRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    carry :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSbbRec = MediumLevelILSbbRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    carry :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRlcRec = MediumLevelILRlcRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    carry :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRrcRec = MediumLevelILRrcRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    carry :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarRec = MediumLevelILSetVarRec
  { dest :: BNVariable,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarFieldRec = MediumLevelILSetVarFieldRec
  { dest :: BNVariable,
    offset :: Int,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarSplitRec = MediumLevelILSetVarSplitRec
  { high :: BNVariable,
    low :: BNVariable,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAssertRec = MediumLevelILAssertRec
  { src :: BNVariable,
    constraint :: BNPossibleValueSet,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAssertSsaRec = MediumLevelILAssertSsaRec
  { src :: BNSSAVariable,
    constraint :: BNPossibleValueSet,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILForceVerRec = MediumLevelILForceVerRec
  { dest :: BNVariable,
    src :: BNVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILForceVerSsaRec = MediumLevelILForceVerSsaRec
  { dest :: BNSSAVariable,
    src :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLoadRec = MediumLevelILLoadRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILLoadStructRec = MediumLevelILLoadStructRec
  { src :: MediumLevelILSSAInstruction,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILStoreRec = MediumLevelILStoreRec
  { src :: MediumLevelILSSAInstruction,
    dest :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILStoreStructRec = MediumLevelILStoreStructRec
  { dest :: MediumLevelILSSAInstruction,
    offset :: Int,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarRec = MediumLevelILVarRec
  { src :: BNVariable,
    var :: BNVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarFieldRec = MediumLevelILVarFieldRec
  { src :: BNVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarSplitRec = MediumLevelILVarSplitRec
  { high :: BNVariable,
    low :: BNVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILExternPtrRec = MediumLevelILExternPtrRec
  { constant :: Int,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRetHintRec = MediumLevelILRetHintRec
  { dest :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCallRec = MediumLevelILCallRec
  { output :: [BNVariable],
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILBpRec = MediumLevelILBpRec
  {core :: CoreMediumLevelILInstruction}
  deriving (Show, Eq, Ord)

data MediumLevelILTrapRec = MediumLevelILTrapRec
  { vector :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILUndefRec = MediumLevelILUndefRec
  {core :: CoreMediumLevelILInstruction}
  deriving (Show, Eq, Ord)

data MediumLevelILUnimplRec = MediumLevelILUnimplRec
  {core :: CoreMediumLevelILInstruction}
  deriving (Show, Eq, Ord)

data MediumLevelILUnimplMemRec = MediumLevelILUnimplMemRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSetVarSplitSsaRec = MediumLevelILSetVarSplitSsaRec
  { high :: BNSSAVariable,
    low :: BNSSAVariable,
    src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarSplitSsaRec = MediumLevelILVarSplitSsaRec
  { high :: BNSSAVariable,
    low :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCallUntypedSsaRec = MediumLevelILCallUntypedSsaRec
  { output :: [BNSSAVariable],
    outputDestMemory :: Int,
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    paramsSrcMemory :: Int,
    stack :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCallUntypedRec = MediumLevelILCallUntypedRec
  { output :: [BNVariable],
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    stack :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSeparateParamListRec = MediumLevelILSeparateParamListRec
  { params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSharedParamSlotRec = MediumLevelILSharedParamSlotRec
  { params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarOutputRec = MediumLevelILVarOutputRec
  { dest :: BNVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarOutputFieldRec = MediumLevelILVarOutputFieldRec
  { dest :: BNVariable,
    offset :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILStoreOutputRec = MediumLevelILStoreOutputRec
  { dest :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSyscallRec = MediumLevelILSyscallRec
  { output :: [BNVariable],
    params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSyscallUntypedRec = MediumLevelILSyscallUntypedRec
  { output :: [BNVariable],
    params :: [MediumLevelILSSAInstruction],
    stack :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILTailcallRec = MediumLevelILTailcallRec
  { output :: [BNVariable],
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILTailcallUntypedRec = MediumLevelILTailcallUntypedRec
  { output :: [BNVariable],
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    stack :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFreeVarSlotRec = MediumLevelILFreeVarSlotRec
  { dest :: BNVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSyscallSsaRec = MediumLevelILSyscallSsaRec
  { output :: [BNSSAVariable],
    outputDestMemory :: Int,
    params :: [MediumLevelILSSAInstruction],
    srcMemory :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILSyscallUntypedSsaRec = MediumLevelILSyscallUntypedSsaRec
  { output :: [BNSSAVariable],
    outputDestMemory :: Int,
    params :: [MediumLevelILSSAInstruction],
    paramsSrcMemory :: Int,
    stack :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILTailcallUntypedSsaRec = MediumLevelILTailcallUntypedSsaRec
  { output :: [BNSSAVariable],
    outputDestMemory :: Int,
    dest :: MediumLevelILSSAInstruction,
    params :: [MediumLevelILSSAInstruction],
    stack :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMemoryIntrinsicOutputSsaRec = MediumLevelILMemoryIntrinsicOutputSsaRec
  { destMemory :: Int,
    output :: [BNSSAVariable],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMemoryIntrinsicSsaRec = MediumLevelILMemoryIntrinsicSsaRec
  { output :: [BNSSAVariable],
    destMemory :: Int,
    intrinsic :: ILIntrinsic,
    params :: [MediumLevelILSSAInstruction],
    srcMemory :: Int,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILFreeVarSlotSsaRec = MediumLevelILFreeVarSlotSsaRec
  { dest :: BNSSAVariable,
    prev :: BNSSAVariable,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILVarPhiRec = MediumLevelILVarPhiRec
  { dest :: BNSSAVariable,
    src :: [BNSSAVariable],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMemPhiRec = MediumLevelILMemPhiRec
  { destMemory :: Int,
    srcMemory :: [Int],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILBlockToExpandRec = MediumLevelILBlockToExpandRec
  { exprs :: [MediumLevelILSSAInstruction],
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILBswapRec = MediumLevelILBswapRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILPopcntRec = MediumLevelILPopcntRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILClzRec = MediumLevelILClzRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILCtzRec = MediumLevelILCtzRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILRbitRec = MediumLevelILRbitRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILClsRec = MediumLevelILClsRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMinsRec = MediumLevelILMinsRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMaxsRec = MediumLevelILMaxsRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMinuRec = MediumLevelILMinuRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILMaxuRec = MediumLevelILMaxuRec
  { left :: MediumLevelILSSAInstruction,
    right :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data MediumLevelILAbsRec = MediumLevelILAbsRec
  { src :: MediumLevelILSSAInstruction,
    core :: CoreMediumLevelILInstruction
  }
  deriving (Show, Eq, Ord)

data Localcall
  = MediumLevelILCallSsa MediumLevelILCallSsaRec
  | MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec
  deriving (Show, Eq, Ord)

data Constant
  = MediumLevelILConst MediumLevelILConstRec
  | MediumLevelILConstPtr MediumLevelILConstPtrRec
  | MediumLevelILFloatConst MediumLevelILFloatConstRec
  | MediumLevelILImport MediumLevelILImportRec
  | MediumLevelILConstData MediumLevelILConstDataRec
  | MediumLevelILExternPtr MediumLevelILExternPtrRec
  deriving (Show, Eq, Ord)

data Comparison
  = MediumLevelILCmpE MediumLevelILCmpERec
  | MediumLevelILFcmpE MediumLevelILFcmpERec
  | MediumLevelILCmpNe MediumLevelILCmpNeRec
  | MediumLevelILFcmpNe MediumLevelILFcmpNeRec
  | MediumLevelILFcmpLt MediumLevelILFcmpLtRec
  | MediumLevelILFcmpLe MediumLevelILFcmpLeRec
  | MediumLevelILFcmpGe MediumLevelILFcmpGeRec
  | MediumLevelILFcmpGt MediumLevelILFcmpGtRec
  | MediumLevelILCmpSlt MediumLevelILCmpSltRec
  | MediumLevelILCmpUlt MediumLevelILCmpUltRec
  | MediumLevelILCmpSle MediumLevelILCmpSleRec
  | MediumLevelILCmpUle MediumLevelILCmpUleRec
  | MediumLevelILCmpSge MediumLevelILCmpSgeRec
  | MediumLevelILCmpUge MediumLevelILCmpUgeRec
  | MediumLevelILCmpSgt MediumLevelILCmpSgtRec
  | MediumLevelILCmpUgt MediumLevelILCmpUgtRec
  | MediumLevelILFcmpO MediumLevelILFcmpORec
  | MediumLevelILFcmpUo MediumLevelILFcmpUoRec
  | MediumLevelILTestBit MediumLevelILTestBitRec
  deriving (Show, Eq, Ord)

data Arithmetic
  = MediumLevelILNeg MediumLevelILNegRec
  | MediumLevelILNot MediumLevelILNotRec
  | MediumLevelILSx MediumLevelILSxRec
  | MediumLevelILZx MediumLevelILZxRec
  | MediumLevelILLowPart MediumLevelILLowPartRec
  | MediumLevelILFsqrt MediumLevelILFsqrtRec
  | MediumLevelILFneg MediumLevelILFnegRec
  | MediumLevelILFabs MediumLevelILFabsRec
  | MediumLevelILFloatToInt MediumLevelILFloatToIntRec
  | MediumLevelILIntToFloat MediumLevelILIntToFloatRec
  | MediumLevelILFloatConv MediumLevelILFloatConvRec
  | MediumLevelILRoundToInt MediumLevelILRoundToIntRec
  | MediumLevelILFloor MediumLevelILFloorRec
  | MediumLevelILCeil MediumLevelILCeilRec
  | MediumLevelILFtrunc MediumLevelILFtruncRec
  | MediumLevelILAdd MediumLevelILAddRec
  | MediumLevelILSub MediumLevelILSubRec
  | MediumLevelILAnd MediumLevelILAndRec
  | MediumLevelILOr MediumLevelILOrRec
  | MediumLevelILXor MediumLevelILXorRec
  | MediumLevelILLsl MediumLevelILLslRec
  | MediumLevelILLsr MediumLevelILLsrRec
  | MediumLevelILAsr MediumLevelILAsrRec
  | MediumLevelILRol MediumLevelILRolRec
  | MediumLevelILRor MediumLevelILRorRec
  | MediumLevelILMul MediumLevelILMulRec
  | MediumLevelILDivu MediumLevelILDivuRec
  | MediumLevelILDivs MediumLevelILDivsRec
  | MediumLevelILModu MediumLevelILModuRec
  | MediumLevelILMods MediumLevelILModsRec
  | MediumLevelILAddOverflow MediumLevelILAddOverflowRec
  | MediumLevelILFadd MediumLevelILFaddRec
  | MediumLevelILFsub MediumLevelILFsubRec
  | MediumLevelILFmul MediumLevelILFmulRec
  | MediumLevelILFdiv MediumLevelILFdivRec
  | MediumLevelILBswap MediumLevelILBswapRec
  | MediumLevelILPopcnt MediumLevelILPopcntRec
  | MediumLevelILClz MediumLevelILClzRec
  | MediumLevelILCtz MediumLevelILCtzRec
  | MediumLevelILRbit MediumLevelILRbitRec
  | MediumLevelILCls MediumLevelILClsRec
  | MediumLevelILMins MediumLevelILMinsRec
  | MediumLevelILMaxs MediumLevelILMaxsRec
  | MediumLevelILMinu MediumLevelILMinuRec
  | MediumLevelILMaxu MediumLevelILMaxuRec
  | MediumLevelILAbs MediumLevelILAbsRec
  deriving (Show, Eq, Ord)

data Terminal
  = MediumLevelILNoret MediumLevelILNoretRec
  | MediumLevelILBp MediumLevelILBpRec
  | MediumLevelILJump MediumLevelILJumpRec
  | MediumLevelILGoto MediumLevelILGotoRec
  | MediumLevelILTrap MediumLevelILTrapRec
  | MediumLevelILJumpTo MediumLevelILJumpToRec
  | MediumLevelILIf MediumLevelILIfRec
  deriving (Show, Eq, Ord)

data Syscall
  = MediumLevelILSyscallUntyped MediumLevelILSyscallUntypedRec
  | MediumLevelILSyscallSsa MediumLevelILSyscallSsaRec
  | MediumLevelILSyscall MediumLevelILSyscallRec
  | MediumLevelILSyscallUntypedSsa MediumLevelILSyscallUntypedSsaRec
  deriving (Show, Eq, Ord)

data Tailcall
  = MediumLevelILTailcallUntyped MediumLevelILTailcallUntypedRec
  | MediumLevelILTailcall MediumLevelILTailcallRec
  | MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec
  | MediumLevelILTailcallUntypedSsa MediumLevelILTailcallUntypedSsaRec
  deriving (Show, Eq, Ord)

data ControlFlow
  = MediumLevelILRetHint MediumLevelILRetHintRec
  deriving (Show, Eq, Ord)

data Return
  = MediumLevelILRet MediumLevelILRetRec
  deriving (Show, Eq, Ord)

data Load
  = MediumLevelILLoad MediumLevelILLoadRec
  | MediumLevelILLoadStruct MediumLevelILLoadStructRec
  | MediumLevelILLoadSsa MediumLevelILLoadSsaRec
  | MediumLevelILLoadStructSsa MediumLevelILLoadStructSsaRec
  deriving (Show, Eq, Ord)

data Store
  = MediumLevelILStore MediumLevelILStoreRec
  | MediumLevelILStoreStruct MediumLevelILStoreStructRec
  | MediumLevelILStoreSsa MediumLevelILStoreSsaRec
  | MediumLevelILStoreStructSsa MediumLevelILStoreStructSsaRec
  | MediumLevelILStoreOutput MediumLevelILStoreOutputRec
  deriving (Show, Eq, Ord)

data Memory
  = MediumLevelILUnimplMem MediumLevelILUnimplMemRec
  | MediumLevelILMemPhi MediumLevelILMemPhiRec
  deriving (Show, Eq, Ord)

data Carry
  = MediumLevelILAdc MediumLevelILAdcRec
  | MediumLevelILSbb MediumLevelILSbbRec
  | MediumLevelILRlc MediumLevelILRlcRec
  | MediumLevelILRrc MediumLevelILRrcRec
  deriving (Show, Eq, Ord)

data SetVar
  = MediumLevelILSetVar MediumLevelILSetVarRec
  | MediumLevelILVarPhi MediumLevelILVarPhiRec
  | MediumLevelILSetVarSsa MediumLevelILSetVarSsaRec
  | MediumLevelILSetVarAliased MediumLevelILSetVarAliasedRec
  | MediumLevelILSetVarSsaField MediumLevelILSetVarSsaFieldRec
  | MediumLevelILSetVarSplitSsa MediumLevelILSetVarSplitSsaRec
  | MediumLevelILSetVarAliasedField MediumLevelILSetVarAliasedFieldRec
  | MediumLevelILSetVarField MediumLevelILSetVarFieldRec
  | MediumLevelILSetVarSplit MediumLevelILSetVarSplitRec
  | MediumLevelILVarOutputField MediumLevelILVarOutputFieldRec
  | MediumLevelILVarOutputSsaField MediumLevelILVarOutputSsaFieldRec
  | MediumLevelILVarOutputAliased MediumLevelILVarOutputAliasedRec
  | MediumLevelILVarOutputAliasedField MediumLevelILVarOutputAliasedFieldRec
  deriving (Show, Eq, Ord)

data RegisterStack
  = MediumLevelILFreeVarSlot MediumLevelILFreeVarSlotRec
  | MediumLevelILFreeVarSlotSsa MediumLevelILFreeVarSlotSsaRec
  | MediumLevelILVarOutput MediumLevelILVarOutputRec
  deriving (Show, Eq, Ord)

data VariableInstruction
  = MediumLevelILVar MediumLevelILVarRec
  | MediumLevelILVarSsa MediumLevelILVarSsaRec
  | MediumLevelILVarAliased MediumLevelILVarAliasedRec
  | MediumLevelILVarSsaField MediumLevelILVarSsaFieldRec
  | MediumLevelILVarAliasedField MediumLevelILVarAliasedFieldRec
  | MediumLevelILVarSplitSsa MediumLevelILVarSplitSsaRec
  deriving (Show, Eq, Ord)

data IntrinsicInstruction
  = MediumLevelILIntrinsic MediumLevelILIntrinsicRec
  | MediumLevelILIntrinsicSsa MediumLevelILIntrinsicSsaRec
  | MediumLevelILMemoryIntrinsicSsa MediumLevelILMemoryIntrinsicSsaRec
  deriving (Show, Eq, Ord)

data MediumLevelILSSAInstruction
  = Localcall Localcall
  | Constant Constant
  | Comparison Comparison
  | Arithmetic Arithmetic
  | Terminal Terminal
  | Syscall Syscall
  | Tailcall Tailcall
  | ControlFlow ControlFlow
  | Return Return
  | Load Load
  | Store Store
  | Memory Memory
  | Carry Carry
  | VariableInstruction VariableInstruction
  | SetVar SetVar
  | RegisterStack RegisterStack
  | IntrinsicInstruction IntrinsicInstruction
  | MediumLevelILCallOutputSsa MediumLevelILCallOutputSsaRec
  | MediumLevelILMemoryIntrinsicOutputSsa MediumLevelILMemoryIntrinsicOutputSsaRec
  | MediumLevelILCallParamSsa MediumLevelILCallParamSsaRec
  | MediumLevelILCallParam MediumLevelILCallParamRec
  | MediumLevelILNop MediumLevelILNopRec
  | MediumLevelILAddressOf MediumLevelILAddressOfRec
  | MediumLevelILAddressOfField MediumLevelILAddressOfFieldRec
  | MediumLevelILPassByRef MediumLevelILPassByRefRec
  | MediumLevelILReturnByRef MediumLevelILReturnByRefRec
  | MediumLevelILMuluDp MediumLevelILMuluDpRec
  | MediumLevelILMulsDp MediumLevelILMulsDpRec
  | MediumLevelILDivuDp MediumLevelILDivuDpRec
  | MediumLevelILDivsDp MediumLevelILDivsDpRec
  | MediumLevelILModuDp MediumLevelILModuDpRec
  | MediumLevelILModsDp MediumLevelILModsDpRec
  | MediumLevelILBoolToInt MediumLevelILBoolToIntRec
  | MediumLevelILAssert MediumLevelILAssertRec
  | MediumLevelILAssertSsa MediumLevelILAssertSsaRec
  | MediumLevelILForceVer MediumLevelILForceVerRec
  | MediumLevelILForceVerSsa MediumLevelILForceVerSsaRec
  | MediumLevelILVarField MediumLevelILVarFieldRec
  | MediumLevelILVarSplit MediumLevelILVarSplitRec
  | MediumLevelILUndef MediumLevelILUndefRec
  | MediumLevelILUnimpl MediumLevelILUnimplRec
  | MediumLevelILSeparateParamList MediumLevelILSeparateParamListRec
  | MediumLevelILSharedParamSlot MediumLevelILSharedParamSlotRec
  | MediumLevelILVarOutputSsa MediumLevelILVarOutputSsaRec
  | MediumLevelILBlockToExpand MediumLevelILBlockToExpandRec
  deriving (Show, Eq, Ord)
