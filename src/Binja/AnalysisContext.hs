{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Binja.AnalysisContext
-- Description : Central abstraction
-- License     : MIT
-- Maintainer  : hello@bloombit.dev
-- Stability   : alpha
--
-- @Binja.AnalysisContext@ extracts and lifts low level types from binary ninja into Beluga's central
-- abstraction. This is the recommended interface for most users.
--
-- [/Reasons not to use:/]
--
-- * Less data than AnalysisContext provides is required and have limited hardware.
-- * AnalysisContext is fixed to the SSA variant of Medium Level IL.
--
-- [/Reasons to use:/]
--
-- * Extracts and lifts the common types required by most program analysis in a single call.
-- * Abstracts away many low level FFI calls and types.
-- * Creates a single type that can be queried in pure functions (no further IO calls required for most analysis).
--   This lends itself to making things easier in creating parallel code.
module Binja.AnalysisContext
  ( Binja.AnalysisContext.create,
    Binja.AnalysisContext.symbolAt,
    Binja.AnalysisContext.callers,
    Binja.AnalysisContext.extractCallDestSymbol,
    Binja.AnalysisContext.close,
  )
where

import Binja.BinaryView
import Binja.ControlFlowGraph
import Binja.Function
import Binja.Mlil
import Binja.Types
import Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set as Set

-- |
--
-- Derive an AnalysisContext from a given filename and json-formatted binja options.
--
-- Warning: every function contains a MLIL SSA variant; otherwise this function will
-- throw an exception.
--
-- Suggested minimum settings:
--
--   * Set analysis.mode.maxFunctionSize to 0 (disables max function size)
--   * Set analysis.mode.maxFunctionAnalysisTime to 0 (disables timeouts)
--   * Set analysis.mode` to intermediate to disable HLIL generation
create ::
  -- | Filename to an executable or an existing binja database (bndb)
  String ->
  -- | Options in json format
  String ->
  IO AnalysisContext
create filename options = do
  viewHandle' <- Binja.BinaryView.load filename options
  functions' <- Binja.BinaryView.functions viewHandle'
  functionContexts <- mapM createFunctionContext functions'
  entryFunction' <- Binja.BinaryView.entryFunction viewHandle'
  entryFunctionContext <- traverse createFunctionContext entryFunction'
  entryFunctions' <- Binja.BinaryView.entryFunctions viewHandle'
  entryFunctionContexts <- mapM createFunctionContext entryFunctions'
  symbols' <- Binja.BinaryView.symbols viewHandle'
  strings' <- catMaybes <$> Binja.BinaryView.strings viewHandle'
  pure
    AnalysisContext
      { viewHandle = viewHandle',
        functions = functionContexts,
        entryFunction = entryFunctionContext,
        entryFunctions = entryFunctionContexts,
        symbols = symbols',
        strings = strings'
      }

createFunctionContext :: BNFunctionPtr -> IO FunctionContext
createFunctionContext handle' = do
  mlilSSAHandle <- Binja.Function.mlilSSA handle'
  start' <- Binja.Function.start handle'
  symbol' <- Binja.Function.symbol handle'
  auto' <- Binja.Function.auto handle'
  instructions' <- Binja.Mlil.instructionsFromFuncNoChildren mlilSSAHandle
  ssaVariables' <- Binja.Function.ssaVars mlilSSAHandle
  ssaVarContext' <- Map.fromList <$> mapM (\l -> createSSAVariableContext l mlilSSAHandle) ssaVariables'
  aliasedVars' <- Binja.Function.aliasedVars mlilSSAHandle
  parameterVars' <- Binja.Function.parameterVars mlilSSAHandle
  architecture' <- Binja.Function.architecture mlilSSAHandle
  cfg' <- Binja.ControlFlowGraph.create mlilSSAHandle
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
        architecture = architecture',
        cfg = cfg'
      }

createSSAVariableContext :: BNSSAVariable -> BNMlilSSAFunctionPtr -> IO (BNSSAVariable, SSAVariableContext)
createSSAVariableContext var' func = do
  defSite' <- Binja.Mlil.defSite var' func
  useSites' <- Binja.Mlil.useSites var' func
  pure $ (var', SSAVariableContext {defSite = defSite', useSites = useSites'})

-- | Acquire the symbol at address if one exists.
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

-- |
--  Given a call instruction attempt to recover the destination symbol (symbol that is called).
--  There are many patterns that could occur. Currently only constant destinations are supported.
--  In the future a cocktail of patterns will be supported. Further reading: <https://dl.acm.org/doi/10.1145/3622833 A Cocktail Approach to Practical Call Graph Construction>
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

-- |
-- Given a function context iterate all instructions to:
--
--   * Find call instructions
--   * Resolve symbols which are called when possible via extractCallDestSymbol
--
-- __Assumption__: It is assumed the function context is present in the functions
-- field of AnalysisContext.
callers :: AnalysisContext -> FunctionContext -> Set.Set Symbol
callers analysisContext functionContext =
  Set.fromList $
    catMaybes $
      Prelude.map (Binja.AnalysisContext.extractCallDestSymbol analysisContext) $
        Prelude.filter isCall $
          concat $
            Prelude.map Binja.Mlil.children $
              Binja.Types.instructions functionContext
  where
    isCall :: MediumLevelILSSAInstruction -> Bool
    isCall (Localcall _) = True
    isCall (Tailcall _) = True
    isCall (Syscall _) = True
    isCall _ = False

-- |
--  Must be called once finished with an AnalysisContext to avoid handle leak.
--  Suggested pattern: <https://wiki.haskell.org/Bracket_pattern Bracket Pattern>
close :: AnalysisContext -> IO ()
close = Binja.BinaryView.close . viewHandle
