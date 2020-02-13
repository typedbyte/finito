<p align="center">
<img src="./logo.png">
</p>

# finito

[![Hackage](https://img.shields.io/hackage/v/finito.svg?logo=haskell&label=finito)](https://hackage.haskell.org/package/finito)

A constraint solver for finite domains, written in Haskell. The implementation is based on propagators and cells holding ranges of possible integer values (using the packages [propeller](https://github.com/typedbyte/propeller) and [numeric-domains](https://github.com/typedbyte/numeric-domains)).

[Sudoku](./examples/sudoku/Main.hs) and the [n-queens problem](./examples/queens/Main.hs) with configurable board size are provided as examples on how to use the library. In addition, the [Hackage documentation](https://hackage.haskell.org/package/finito) is quite compact and covers all the provided types and functions.