# beluga<img width="153" height="160" alt="Screenshot_2026-01-04_at_12 25 04_AM-removebg-preview" src="https://github.com/user-attachments/assets/88c1f6a6-4560-48be-bddc-969206510bb0" />

Program analysis and bindings in Haskell for binary ninja with support for the medium level SSA intermediate language.

## Build Instructions
Symlink binary ninja core shared objects to package root.

For example on MacOS:

- ```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS/libbinaryninjacore.dylib libbinaryninjacore.dylib```
- ```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS/libbinaryninjacore.1.dylib libbinaryninjacore.1.dylib```

Or pass stack the directory where libbinaryninjacore is located.

Then build and exec the demo (after changing the path to your license and binary/bndb of interest) with stack.
- stack init
- stack build --extra-lib-dirs "$(pwd)"
- stack run example --extra-lib-dirs "$(pwd)" -- +RTS -N14 -sstats -RTS

## Documentation

Haddock-generated documentation is hosted [here](https://bloombit.dev/documentation/beluga/index.html).

## Branches
Tested and implemented against an ultimate license install.

The main branch tracks the current work-in-progress.

## Format
All haskell files are formatted with https://github.com/tweag/ormolu

