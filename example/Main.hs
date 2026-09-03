module Main where

import Binja.AnalysisContext
import Binja.ControlFlowGraph
import Binja.FFI (getVersionString, shutdown)
import Binja.Types.Core
import Control.Monad (forM_)
import Data.Char (isSpace)
import System.IO
import Prelude hiding (log)

filenames :: [String]
filenames =
  [ -- "./test/macos/cmake",
    -- "./test/macos/libsignal-client.node",
    -- "./test/macos/d8",
    -- "./test/macos/sudo.bndb",
    -- "./test/qcadsp8380.mbn"
    -- "./test/macos/chrome",
    -- "./test/macos/git",
    -- "./test/macos/llvm18/clang-18",
    -- "./test/macos/llvm18/libLLVMDemangle.a",
    -- "./test/macos/llvm18/mlir-opt",
    "./test/macos/webkit-304137@main/libANGLE-shared.dylib.bndb"
    -- "./test/macos/webkit-304137@main/minidom",
    -- "./test/macos/webkit-304137@main/libWebCoreTestSupport.dylib",
    -- "./test/macos/webkit-304137@main/libwebrtc.dylib"
  ]

-- Derive a filename without spaces from binja version
sanitizeVersion :: String -> String
sanitizeVersion = map $ \c -> if isSpace c then '_' else c

main :: IO ()
main = do
  let options =
        "{\"analysis.mode\": \"intermediate\","
          ++ "\"analysis.limits.maxFunctionSize\": 0,"
          ++ "\"analysis.limits.maxFunctionAnalysisTime\": 0}"
  putStrLn "[*] Running tests..."
  version' <- getVersionString
  let testFilePath = "./test/reports/" ++ (sanitizeVersion version') ++ ".txt"
  withFile testFilePath WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    let log s = hPutStrLn h s
    log $ " [*] Version: " ++ version'
    forM_ filenames $ \fname -> do
      putStrLn $ " [*] Processing: " ++ fname
      context <- Binja.AnalysisContext.create fname options
      let functionCount = length $ functions context
      let entryFunctionCount = length $ entryFunctions context
      let symbolCount = length $ symbols context
      let stringCount = length $ strings context
      let blockCount = sum $ Prelude.map (order . cfg) $ functions context
      let totalEdges = sum $ Prelude.map (size . cfg) $ functions context
      log $ "   [*] Function count: " ++ (show functionCount)
      log $ "   [*] Entry function count: " ++ (show entryFunctionCount)
      log $ "   [*] Symbol count: " ++ (show symbolCount)
      log $ "   [*] String count: " ++ (show stringCount)
      log $ "   [*] Basic block count: " ++ (show blockCount)
      log $ "   [*] Total block edge count: " ++ (show totalEdges)
      log " [*] Processing complete."
      Binja.AnalysisContext.close context
  shutdown
