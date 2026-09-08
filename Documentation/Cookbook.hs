{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Binja.AnalysisContext
import Binja.BasicBlock
import Binja.BinaryView
import Binja.ControlFlowGraph
import Binja.Types.Core
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  -- \| Load a binary or Binja Database
  let options =
        "{\"analysis.mode\": \"intermediate\","
          ++ "\"analysis.limits.maxFunctionSize\": 0,"
          ++ "\"analysis.limits.maxFunctionAnalysisTime\": 0}"
  let fname = "./test/android/tegu-bp3a.251105.015/lib64/libjson.so"
  context <- Binja.AnalysisContext.create fname options

  -- \| Getting all functions in a binary
  let funcs = (\AnalysisContext {functions = functions'} -> functions') context
  _ <- mapM (Prelude.print . show) funcs

  -- \| Getting a specific function
  let findByName searchStr FunctionContext {symbol = Symbol {name = symName}} = symName == searchStr
  let findByAddress searchAddress FunctionContext {symbol = Symbol {address = symAddress}} = symAddress == searchAddress
  Prelude.print "Functions with name: json_tokener_parse_ex:"
  Prelude.print $ show $ Prelude.filter (findByName "json_tokener_parse_ex") funcs
  Prelude.print "Function at 0x4100f0:"
  Prelude.print $ show $ Prelude.filter (findByAddress 0x4100f0) funcs
  Prelude.print "Functions containing 0x4100f0:"
  Prelude.print $ show $ Binja.AnalysisContext.contains context 0x4100f0

  -- \| Finding the largest function (by most bytes)
  -- Note that the length of a basic block differs depending on IL and if in ssa form.
  let functionLength FunctionContext {cfg = cfg'} = sum $ map (\BasicBlockMlilSSA {startAddress = start', endAddress = end'} -> end' - start') $ blocks cfg'
  putStr $ "Largest function by bytes: "
  Prelude.print $ show $ maximumBy (comparing functionLength) funcs

  -- \| All top-level medium level IL ssa instructions in a binary
  let allTopLevelInsts = Binja.AnalysisContext.topLevelInstructions context

  -- \| All medium level IL ssa instructions in a binary
  let allInsts = Binja.AnalysisContext.instructions context

  -- \| Call Graph Analysis

  -- \| All callers of a function
  let interestingFuncs = Prelude.filter (findByAddress 0x40fe80) funcs
  _ <- case interestingFuncs of
    [] -> error "Interesting function not found."
    (hd : tl) -> do
      Prelude.print $ "Callers of: " ++ show hd
      Prelude.print $ show $ Binja.AnalysisContext.callers context hd

  -- \| All instructions that call a function
  _ <- case interestingFuncs of
    [] -> error "Interesting function not found."
    (hd : tl) -> do
      Prelude.print $ "Caller sites of " ++ show hd
      Prelude.print $ show $ Binja.AnalysisContext.callerSites context hd

  -- \| All call instructions in a function
  let jsonObjectDoubleToJsonStringFormat = Prelude.filter (findByAddress 0x409dc0) funcs
  _ <- case jsonObjectDoubleToJsonStringFormat of
    [] -> error "Function not found."
    (hd : tl) -> do
      Prelude.print $ "Call instructions of " ++ show hd
      Prelude.print $ show $ length $ Binja.AnalysisContext.callInstructions hd

  -- \| All callees of a function (functions that get called by a given function)
  _ <- case jsonObjectDoubleToJsonStringFormat of
    [] -> error "Function not found."
    (hd : tl) -> do
      Prelude.print $ "Callees of " ++ show hd
      Prelude.print $ show $ Binja.AnalysisContext.callees context hd

  Binja.AnalysisContext.close context
