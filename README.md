# binaryninja-haskell
Haskell bindings for binary ninja with support for the medium level SSA intermediate language.

## Build Instructions
Symlink binary ninja core shared objects to package root.

For example on MacOS:

- ```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS/libbinaryninjacore.dylib libbinaryninjacore.dylib```
- ```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS/libbinaryninjacore.1.dylib libbinaryninjacore.1.dylib```

Or pass stack the directory where libbinaryninjacore is located.

Then build and exec the demo (after changing the path to your license and binary/bndb of interest) with stack.
- stack init
- stack build --extra-lib-dirs "$(pwd)"
- stack run example --extra-lib-dirs "$(pwd)"

## Branches
Tested and implemented against an ultimate license install.

The main branch tracks the current work-in-progress.

## Format
All haskell files are formatted with https://github.com/tweag/ormolu

