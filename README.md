# Suntheme

Runs a script on sunrise and sunset, written in pure Haskell

---

You may be wondering how a program written in Haskell, the purely functional
programming language, could possibly act on the real world by running a so-called "script."

It's simple. We take in the entire World as an input to a pure function, the IO Monad.
It then maps the original World to a new (generated) World, with our desired IO actions carefully applied with mathematical precision.

> From the second perspective, an IO action transforms the whole world. IO actions are actually pure, because they receive a unique world as an argument and then return the changed world.

See [this](https://lean-lang.org/functional_programming_in_lean/monads/io.html) for more information.

Furthermore, you may wonder how this `suntheme` could possibly be used on a
_purely functional_ Linux distribution, like NixOS. Wouldn't mutating the
system colorscheme be pathologically changing the system state? This is simple
as well. We simply utilize the NixOS specialisations feature to generate _two
copies_ of every single generation, one in dark and one in light theme, and
then _swap out the entire system_. Therefore no state is mutated and the user
simply gets swapped onto a specialized generation of NixOS with their desired
colorscheme. Behold: the genius of purely functional programming.

Work is ongoing to create a turnkey drop-in NixOS module that interfaces with
Stylix to automatically create the system color activation scripts and expose
them to `suntheme`.

## Hacking on suntheme

It's trivial to get started with suntheme development thanks to [Nix](https://nixos.org/), the purely functional package manager.
Naturally, we leverage it as our primary package manager, both for Hackage and development tools like language servers and the like.

First, install Nix through your preferred avenue or local system administrator. If unsure, we recommend [the Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer).
Make sure flakes and nix-command are enabled (the Determinate Installer will enable them by default).

Once you have `nix`, simply type:

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
