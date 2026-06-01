module Main where

import Binja.AnalysisContext
import Binja.FFI
import Binja.Types
import Binja.Utils
import Control.Monad (forM_)

filenames :: [String]
filenames =
  [ "./test/macos/cmake",
    "./test/macos/libsignal-client.node",
    "./test/macos/d8",
    "./test/macos/sudo.bndb",
    "./test/qcadsp8380.mbn",
    "./test/macos/chrome",
    "./test/macos/git",
    "./test/macos/llvm18/clang-18",
    "./test/macos/llvm18/libLLVMDemangle.a",
    "./test/macos/llvm18/mlir-opt",
    "./test/macos/webkit-304137@main/libANGLE-shared.dylib.bndb",
    "./test/macos/webkit-304137@main/minidom",
    "./test/macos/webkit-304137@main/libWebCoreTestSupport.dylib",
    "./test/macos/webkit-304137@main/libwebrtc.dylib"
  ]

main :: IO ()
main = do
  let options =
        "{\"analysis.mode\": \"intermediate\","
          ++ "\"analysis.limits.maxFunctionSize\": 0,"
          ++ "\"analysis.limits.maxFunctionAnalysisTime\": 0}"
  version' <- getVersionString
  installDirCStr <- c_BNGetInstallDirectory
  installDir <- peekCString installDirCStr
  pluginDirC <- c_BNGetBundledPluginDirectory
  pluginDir <- peekCString pluginDirC
  userDirC <- c_BNGetUserDirectory
  userDir <- peekCString userDirC
  putStrLn $ "[" ++ yellow "*" ++ "] Version: " ++ red version'
  putStrLn $ "[" ++ yellow "*" ++ "] Install Directory: " ++ magenta installDir
  putStrLn $ "[" ++ yellow "*" ++ "] Plugin Directory: " ++ magenta pluginDir
  putStrLn $ "[" ++ yellow "*" ++ "] User Directory: " ++ magenta userDir
  putStrLn $ "[" ++ yellow "*" ++ "] Running tests."
  forM_ filenames $ \fname -> do
    putStrLn $ "[" ++ yellow "*" ++ "] " ++ cyan "Processing: " ++ blue fname
    context <- Binja.AnalysisContext.create fname options
    putStr $ summary context
    Binja.AnalysisContext.close context
  putStrLn $ "[" ++ yellow "*" ++ "]" ++ cyan " Processing " ++ blue "complete."
  shutdown
