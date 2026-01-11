{-# LANGUAGE DuplicateRecordFields #-}

module Binja.AnalysisContext
  ( Binja.AnalysisContext.create,
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
  parameterVars' <- Binja.Function.parameterVars mlilSSAHandle
  -- TODO (1) obtain more aliased variables.
  --          the union of aliased variables, parameter variables and ssa variables
  --          are all variables for function.
  pure
    FunctionContext
      { handle = mlilSSAHandle,
        start = start',
        symbol = symbol',
        auto = auto',
        ssaVars = ssaVarContext',
        parameterVars = parameterVars',
        -- aliasedVars = aliasedVars',
        instructions = instructions'
      }

createSSAVariableContext :: BNSSAVariable -> BNMlilSSAFunctionPtr -> IO (BNSSAVariable, SSAVariableContext)
createSSAVariableContext var' func = do
  defSite' <- Binja.Mlil.defSite var' func
  useSites' <- Binja.Mlil.useSites var' func
  pure $ (var', SSAVariableContext {defSite = defSite', useSites = useSites'})

close :: AnalysisContext -> IO ()
close = Binja.BinaryView.close . viewHandle
