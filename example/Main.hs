module Main where

import Binja.BinaryView
import Binja.FFI (shutdown)
import Callgraph
import Control.Monad (forM_)
import Data.Map as Map
import Data.Set as Set

filenames :: [String]
filenames =
  [ "./test/macos/cmake",
    "./test/macos/libsignal-client.node",
    "./test/macos/d8",
    "./test/macos/sudo",
    "./test/macos/chrome",
    "./test/macos/git",
    "./test/macos/python3",
    "./test/macos/llvm18/clang-18",
    "./test/macos/llvm18/libLLVMDemangle.a",
    "./test/macos/llvm18/mlir-opt",
    "./test/macos/webkit-304137@main/libANGLE-shared.dylib",
    "./test/macos/webkit-304137@main/minidom",
    "./test/macos/webkit-304137@main/libWebCoreTestSupport.dylib",
    "./test/macos/webkit-304137@main/libwebrtc.dylib"
  ]

main :: IO ()
main = do
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0, \"analysis.limits.maxFunctionAnalysisTime\": 0}"
  putStrLn "[*] Running tests..."
  forM_ filenames $ \fname -> do
    putStrLn $ " === [!] Processing: " ++ fname ++ " ==="
    view <- load fname options
    putStrLn $ "  [+] Loaded: " ++ fname
    graph <- Callgraph.create view
    printGraph graph
    Binja.BinaryView.close view
  Prelude.print "[*] Test success."
  shutdown
  where
    printGraph :: Graph -> IO ()
    printGraph =
      mapM_ printEntry . Map.toList
      where
        printEntry :: (Vertex, Set.Set Vertex) -> IO ()
        printEntry (parent, children) = do
          putStrLn (show parent ++ " ->")
          putStrLn "  ["
          mapM_ (putStrLn . ("   " ++) . show) (Set.toList children)
          putStrLn "  ]"
