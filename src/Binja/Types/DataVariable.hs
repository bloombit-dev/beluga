{-# LANGUAGE DuplicateRecordFields #-}

module Binja.Types.DataVariable
  ( DataVariable (..),
    BNTypeClass (..),
    BNBoolWithConfidence (..),
    BNTypeWithConfidence (..),
    BNTypePtr,
  )
where

import qualified Data.ByteString as BS
import Data.Word (Word64, Word8)
import Foreign
  ( Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
    alloca,
  )
import Foreign.C.Types (CBool (..), CSize (..))
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr)

data BNType_

type BNTypePtr = Ptr BNType_

alignmentS :: Int
alignmentS = 8

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

data BNBoolWithConfidence = BNBoolWithConfidence
  { value :: Bool,
    confidence :: Word8
  }
  deriving (Show)

instance Storable BNTypeWithConfidence where
  sizeOf _ = 16
  alignment _ = alignmentS
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

instance Storable BNBoolWithConfidence where
  sizeOf _ = 2
  alignment _ = alignmentS
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

instance Storable DataVariable where
  sizeOf _ = 24
  alignment _ = alignmentS
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
