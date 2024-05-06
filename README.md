# TopHat in Haskell

TopHat is a formalisation of Task Oriented Programming.
This is the Haskell implementation of the language.


## Language and semantics

The language is specified as a GADT in [Task](https://github.com/timjs/tophat-haskell/blob/master/src/Task.hs).
Semantics, including all observations, can be found in [Task.Run](https://github.com/timjs/tophat-haskell/blob/master/src/Task/Run.hs).


## Examples

[app/Main.hs](https://github.com/timjs/tophat-haskell/blob/master/app/Main.hs) includes a couple of examples which can be executed.

## Dependencies

* GHC >= 8.8
* Cabal >= 3.0

## Building

Clone the repository and build it by running Cabal.
You'll need Cabal version 3 for new style builds!

```sh
git clone https://github.com/timjs/tophat-haskell.git
cd tophat-haskell
cabal build
cabal run
```

Tested with GHC-8.8 and Cabal 3.2,
and with GHC-8.10 and Cabal 3.4.
