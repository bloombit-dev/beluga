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

# Minimum Version

This repo requires binary ninja version dev/5.4.9588 Ultimate. Other ultimate versions may work though assume it's not tested.
To upgrade binja version it's advised to compare the results of ```unit_test.sh``` between the known working version
and new version.

## Code Format
All haskell files are formatted with https://github.com/tweag/ormolu

## Related Work and References

The primary inspiration of Beluga is [Frama-C](https://frama-c.com)'s Eva [plugin](https://frama-c.com/fc-plugins/eva.html).
I've found numerous bugs in C codebases with Eva and I want the same experience with binary ninja's MLIL SSA intermediate language.

Frama-c book: [Guide to Software Verification with Frama-C](https://link.springer.com/book/10.1007/978-3-031-55608-1)

Sophia d'Antoine of [Margin Research](https://margin.re) taught a course, [Program Analysis for Vulnerability Research](https://margin.re/training/), which covered abstract interpretation implementation in Binary Ninja.
