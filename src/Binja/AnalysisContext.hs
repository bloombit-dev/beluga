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
  pure
    FunctionContext
      { handle = mlilSSAHandle,
        start = start',
        symbol = symbol',
        auto = auto',
        ssaVars = ssaVariables',
        instructions = instructions'
      }

close :: AnalysisContext -> IO ()
close = Binja.BinaryView.close . viewHandle
