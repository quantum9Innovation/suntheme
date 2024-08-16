# Suntheme

runs a script on sunrise and sunset, written in pure haskell.

You may be wondering how a program written in Haskell, the purely functional
programming language, could possibly act on the real world by running a so-called "script".

It's simple. We take in the entire World as an input to a pure function, the IO Monad.
It then maps the original World to the changed World, with our desired IO actions carefully applied.

> From the second perspective, an IO action transforms the whole world. IO actions are actually pure, because they receive a unique world as an argument and then return the changed world.

See [this](https://lean-lang.org/functional_programming_in_lean/monads/io.html) for more information.
