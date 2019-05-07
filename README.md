# TopHat in Haskell

TopHat is a formalisation of Task Oriented Programming.
This is the Haskell implementation of the language.


## Language and semantics

The language is specified as a GADT in [Data.Task](https://github.com/timjs/tophat-haskell/blob/master/src/Data/Task.hs).
Semantics, including all observations, can be found in [Data.Task.Run](https://github.com/timjs/tophat-haskell/blob/master/src/Data/Task/Run.hs).


## Examples

[Main](https://github.com/timjs/tophat-haskell/blob/master/src/Main.hs) includes a couple of examples which can be executed.


## Building

Clone the repository and build it by running Stack:

```sh
> git clone https://github.com/timjs/tophat-haskell.git
> cd tophat-haskell
> stack build
> stack run
```
