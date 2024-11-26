# Setting up your tools

In this chapter, we'll talk about how you should install Haskell in your system and configure your editor of choice.

## Installation

Start by installing [`ghcup`](https://www.haskell.org/ghcup/). Prefer this over your system package manager (e.g. `brew`, `apt`, etc.) unless you know what you're doing. GHCup is the official version manager for GHC, Cabal, Stack, and HLS. You can think of this like [Rustup](https://rustup.rs/) or [NVM](https://github.com/nvm-sh/nvm).

```bash
# If you don't like running `curl | bash`, see the GHCup website for manual
# installation instructions.
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Once downloaded, you should install the `recommended` versions of `ghc` and `cabal`. Run `ghcup list` to see which versions are `latest` and `recommended` by GHCup. You should install the `recommended` versions. Where they differ, it's usually because tooling support has not caught up to the latest version.

```bash
ghcup install ghc recommended
ghcup install cabal recommended
```

GHC is the main Haskell compiler, Cabal is the main build tool. If you’re familiar with Node.js, some analogies here are:

- GHC ~= Node. It’s not the only Haskell compiler (much like how Node isn’t the only standalone JS runtime), but it’s the one everyone uses.
- Cabal ~= NPM. It’s the build tool most people use, and is the official one.
- Stack ~= Yarn. It’s an alternative build tool to Cabal, and it was a lot better back than Cabal back before Cabal natively supported sandboxes. It does mostly the same things, but used to handle dependencies better. Nowadays, don’t bother - you should prefer Cabal unless you know what you’re doing and need a Stack-specific feature.

HLS is the Haskell Language Server. You usually don’t need to install this standalone, because the most popular code editors’ Haskell plugins usually manage this for you.

## Setting up your editor

Writing Haskell without good IDE support is much more difficult. You should use an officially supported editor:

- For VS Code, you’ll want to install and use the [official Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) (`haskell.haskell`).
- For Emacs, you’ll want to install and use [haskell-mode](https://github.com/haskell/haskell-mode).

Other editors might have good Haskell support, but their plugins are not officially supported by the Haskell.org committee. In a pinch (if you can’t get any other plugins working properly), use [ghcid](https://github.com/ndmitchell/ghcid), which is a simple and dumb daemon that just reloads `ghci` (the Haskell REPL) when file changes are detected.

A functioning editor plugin should give you:

- Type information of expressions on hover. This is the killer feature. It is extremely useful for debugging, and writing Haskell without this functionality is much more annoying.
- Type-driven autocomplete for type holes (we'll talk about these later).
- Inline compilation errors and warnings.
- Symbol autocomplete and import management.
- Symbol renaming.
