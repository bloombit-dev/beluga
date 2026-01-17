{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Binja.AnalysisContext
Description : Central abstraction
License     : MIT
Maintainer  : hello@bloombit.dev
Stability   : alpha

@Binja.AnalysisContext@ extracts and lifts low level types from binary ninja into Beluga's central
abstraction. This is the recommended interface for most users.

[/Reasons not to use:/]

* Less data than AnalysisContext provides is required and have limited hardware.
* AnalysisContext is fixed to the SSA variant of Medium Level IL.

[/Reasons to use:/]

* Extracts and lifts the common types required by most program analysis in a single call.
* Abstracts away many low level FFI calls and types.
* Creates a single type that can be queried in pure functions (no further IO calls required for most analysis).
  This lends itself to making things easier in creating parallel code.
-}

module Binja.AnalysisContext
  ( Binja.AnalysisContext.create,
    Binja.AnalysisContext.symbolAt,
    Binja.AnalysisContext.extractCallDestSymbol,
    Binja.AnalysisContext.close,
  )
where

import Binja.BinaryView
import Binja.Function
import Binja.Mlil
import Binja.Types
import Data.Map as Map
import Data.Maybe (catMaybes)

create :: String -> String -> IO AnalysisContext
create filename options = do
  viewHandle' <- Binja.BinaryView.load filename options
  functions' <- Binja.BinaryView.functions viewHandle'
  functionContexts <- mapM createFunctionContext functions'
  symbols' <- Binja.BinaryView.symbols viewHandle'
  strings' <- catMaybes <$> Binja.BinaryView.strings viewHandle'
  pure
    AnalysisContext
      { viewHandle = viewHandle',
        functions = functionContexts,
        symbols = symbols',
        strings = strings'
      }

createFunctionContext :: BNFunctionPtr -> IO FunctionContext
createFunctionContext handle' = do
  mlilSSAHandle <- Binja.Function.mlilSSA handle'
  mlilHandle <- Binja.Function.mlil handle'
  start' <- Binja.Function.start handle'
  symbol' <- Binja.Function.symbol handle'
  auto' <- Binja.Function.auto handle'
  instructions' <- Binja.Mlil.instructionsFromFuncNoChildren mlilHandle
  ssaVariables' <- Binja.Function.ssaVars mlilSSAHandle
  ssaVarContext' <- Map.fromList <$> mapM (\l -> createSSAVariableContext l mlilSSAHandle) ssaVariables'
  aliasedVars' <- Binja.Function.aliasedVars mlilSSAHandle
  parameterVars' <- Binja.Function.parameterVars mlilSSAHandle
  architecture' <- Binja.Function.architecture mlilSSAHandle
  pure
    FunctionContext
      { handle = mlilSSAHandle,
        start = start',
        symbol = symbol',
        auto = auto',
        ssaVars = ssaVarContext',
        parameterVars = parameterVars',
        aliasedVars = aliasedVars',
        instructions = instructions',
        architecture = architecture'
      }

createSSAVariableContext :: BNSSAVariable -> BNMlilSSAFunctionPtr -> IO (BNSSAVariable, SSAVariableContext)
createSSAVariableContext var' func = do
  defSite' <- Binja.Mlil.defSite var' func
  useSites' <- Binja.Mlil.useSites var' func
  pure $ (var', SSAVariableContext {defSite = defSite', useSites = useSites'})

symbolAt :: AnalysisContext -> Word64 -> Maybe Symbol
symbolAt AnalysisContext {symbols = syms} requestAddr =
  case Prelude.filter ((requestAddr ==) . address) syms of
    [] -> Nothing
    [sym] -> Just sym
    _ -> error $ "Binja.AnalysisContext.symbolAt: Multiple symbols at: " ++ show requestAddr

-- Convert Constant instruction to symbol if possible
constantToSymbol :: AnalysisContext -> Constant -> Maybe Symbol
constantToSymbol context (MediumLevelILConstPtr (MediumLevelILConstPtrRec {constant = c})) = do
  Binja.AnalysisContext.symbolAt context $ fromIntegral c
constantToSymbol context (MediumLevelILImport (MediumLevelILImportRec {constant = c})) = do
  Binja.AnalysisContext.symbolAt context $ fromIntegral c
constantToSymbol _ (MediumLevelILConst (MediumLevelILConstRec {constant = c})) = do
  error $ "Unhandled constant: " ++ show c
constantToSymbol _ (MediumLevelILFloatConst MediumLevelILFloatConstRec {constant = c}) = do
  error $ "Unhandled float constant: " ++ show c
constantToSymbol _ (MediumLevelILConstData MediumLevelILConstDataRec {constant = c}) = do
  error $ "Unhandled constant data: " ++ show c
constantToSymbol context (MediumLevelILExternPtr MediumLevelILExternPtrRec {constant = c}) = do
  Binja.AnalysisContext.symbolAt context $ fromIntegral c

extractCallDestSymbol :: AnalysisContext -> MediumLevelILSSAInstruction -> Maybe Symbol
extractCallDestSymbol context callInst =
  case callInst of
    Localcall lc ->
      case lc of
        (MediumLevelILCall MediumLevelILCallRec {dest = d}) -> processDest d
        (MediumLevelILCallSsa MediumLevelILCallSsaRec {dest = d}) -> processDest d
        (MediumLevelILCallUntypedSsa MediumLevelILCallUntypedSsaRec {dest = d}) -> processDest d
        (MediumLevelILCallUntyped MediumLevelILCallUntypedRec {dest = d}) -> processDest d
    Tailcall tc ->
      case tc of
        (MediumLevelILTailcallUntyped MediumLevelILTailcallUntypedRec {dest = d}) -> processDest d
        (MediumLevelILTailcall MediumLevelILTailcallRec {dest = d}) -> processDest d
        (MediumLevelILTailcallSsa MediumLevelILTailcallSsaRec {dest = d}) -> processDest d
        (MediumLevelILTailcallUntypedSsa MediumLevelILTailcallUntypedSsaRec {dest = d}) -> processDest d
    _ -> error $ "Binja.AnalysisContext.extractCallDestSymbol: unhandled instruction: " ++ show callInst
  where
    processDest :: MediumLevelILSSAInstruction -> Maybe Symbol
    processDest dest' =
      case dest' of
        Constant c -> Binja.AnalysisContext.constantToSymbol context c
        _ -> Nothing

close :: AnalysisContext -> IO ()
close = Binja.BinaryView.close . viewHandle
