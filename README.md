# Suntheme

runs a script on sunrise and sunset, written in pure haskell.

You may be wondering how a program written in Haskell, the purely functional
programming language, could possibly act on the real world by running a so-called "script".

It's simple. We take in the entire World as an input to a pure function, the IO Monad.
It then maps the original World to the changed World, with our desired IO actions carefully applied.

> From the second perspective, an IO action transforms the whole world. IO actions are actually pure, because they receive a unique world as an argument and then return the changed world.

See [this](https://lean-lang.org/functional_programming_in_lean/monads/io.html) for more information.

## Hacking on suntheme

It's trivially easy to get started with suntheme development thanks to [Nix](https://nixos.org/), the purely functional package manager.
Naturally, we leverage it as our primary package manager, for both Hackage packages and development tools like language servers and the like.

First, install Nix through your preferred avenue. If unsure, we recommend [the Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer).
Make sure flakes and nix-command are enabled (the Determinate Installer will enable them by default).

Once you have `nix`, simply type

```bash
nix develop
```

Say yes to any prompts asking you to allow substituters or trust public keys.
Nix will fetch all of the required packages, such as GHC and Hackage dependencies.
Additionally, you will have access to `hlint` and the `haskell-language-server`.

To create a build, type

```bash
nix build
```

A binary will be produced in `result/bin/suntheme`.

To make `suntheme` available in the shell without outputting to `result`, use

```bash
nix shell
```

This will build a binary just like `nix build` but add it temporarily to the PATH, so you can just type `suntheme`.

To build and run `suntheme` immediately without adding it to the PATH, use

```bash
nix run
```

This will build a binary just like `nix shell` but immediately execute it.