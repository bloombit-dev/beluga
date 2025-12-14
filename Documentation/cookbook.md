## Cookbook

### Load a binary or Binja Database
```haskell
module Main where

import Binja.BinaryView
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/bloombit/projects/engine/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  shutdown
```

### Getting all functions in a binary
```haskell
module Main where

import Binja.BinaryView
import Binja.Function
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/bloombit/projects/engine/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  funcs <- Binja.BinaryView.functions view
  mapM_ Binja.Function.print funcs
  Prelude.print $ "Found " ++ show (length funcs) ++ " functions"
  shutdown
```

### Getting all medium level SSA IL instructions
```haskell
module Main where

import Binja.BinaryView
import Binja.Mlil
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/bloombit/projects/engine/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  mlilSSAs <- Binja.Mlil.instructions view
  Prelude.print $ "Found " ++ show (length mlilSSAs) ++ " mlil ssa instructions"
  shutdown
```

### Getting a specific function
```haskell
module Main where

import Binja.BinaryView
import Binja.Function
import Binja.FFI (shutdown)

main = do
  let filename = "/Users/bloombit/projects/engine/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  -- Get functions by name
  funcsByName <- Binja.BinaryView.functionsByName view "-[PhoneViewController _prepareForLoadView]"
  mapM_ Binja.Function.print funcsByName
  -- Get function by address
  funcsByAddr <- Binja.BinaryView.functionsAt view 4294992020
  mapM_ Binja.Function.print funcsByAddr
  -- Get functions containing address
  funcsByContain <- Binja.BinaryView.functionsContaining view 4294992020
  mapM_ Binja.Function.print funcsByName
  shutdown
```

### Getting all code refs of address and associated medium level SSA IL instructions
```haskell
module Main where

import Binja.BinaryView
import Binja.ReferenceSource
import Binja.Mlil
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/bloombit/projects/engine/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  codeRefs' <- Binja.ReferenceSource.codeRefs view 4295938392
  Prelude.print $ "Found " ++ show (length codeRefs') ++ " codeRefs of address 4295938392"
  mlils <- mapM Binja.Mlil.fromRef codeRefs'
  mapM_ Prelude.print mlils
  shutdown
```

### Accessing cross references
```haskell
module Main where

import Binja.BinaryView
import Binja.Function
import Binja.Mlil
import Binja.FFI (shutdown)

main = do
  let filename = "/Users/bloombit/projects/engine/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  -- Get function by address
  iLFuncs <- Binja.BinaryView.functionsAt view 4295449218
  case iLFuncs of
    [] -> Prelude.print "No functions found at 4295449218"
    (hd:_) -> do
      singleFuncMlilSSA <- Binja.Function.mlilSSA hd
      refs' <- Binja.Mlil.callerSites view singleFuncMlilSSA
      Prelude.print $ show $ length refs'
  shutdown
```

### Generate a callgraph for a binary view

The following generates a callgraph without value analysis.
Without value analysis calls to variables, struct fields, etc
will not be considered.

```haskell
module Main where

import Binja.BinaryView
import Binja.FFI (shutdown)
import Callgraph
import Data.Map as Map

main :: IO ()
main = do
  let options = "{ \"analysis.mode\": \"intermediate\", "
                ++ "\"analysis.limits.maxFunctionSize\": 0, "
                ++ "\"analysis.limits.maxFunctionAnalysisTime\": 0 }"
  view <- load "./test/macos/sudo" options
  graph <- Callgraph.create view
  Prelude.print $ "size: " ++ (show $ Map.size graph)
  shutdown
```
