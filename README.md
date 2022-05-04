# Haskell for Working Programmers

_Haskell for Working Programmers_ is a guide for professional programmers to pick up Haskell. Most Haskell learning materials out there swing too hard in an experience direction: they either assume an academic computer science background, or start from complete basics.

This guide is intended for folks working a working, professional knowledge of an existing popular programming language. We’ll skim through common concepts shared with other languages (e.g. "what is a string?"), and learn how to write Haskell effectively by comparing, contrasting, and drawing analogies to other more common languages.

We’ll start by getting your machine set up to build Haskell programs. Then, we’ll compile a program end-to-end and get a "Hello, World" program working. Afterwards, we’ll run through a crash course of Haskell programming concepts. Finally, we’ll put those concepts into practice by building some non-trivial real-world programs.

# Getting set up

## Setting up build tools

Use [ghcup](https://www.haskell.org/ghcup/) to install and manage versions of GHC, Cabal, Stack, and HLS. GHC is the main Haskell compiler, and Cabal is the main build tool. If you’re familiar with Node.js, some analogies here are:

- GHC ~= Node. It’s not the only Haskell compiler (much like how Node isn’t the only standalone JS runtime), but it’s the one everyone uses.
- Cabal ~= NPM. It’s the build tool most people use.
- Stack ~= Yarn. It’s an alternative build tool to Cabal, and it was a lot better back than Cabal back before Cabal natively supported sandboxes. It does mostly the same things, but used to handle dependencies better. Nowadays, don’t bother - you should prefer Cabal unless you know what you’re doing and deliberately need a Stack-specific feature.

HLS is the Haskell Language Server. You usually don’t need to install this standalone, because the most popular code editors’ Haskell plugins usually manage this for you.

You’ll want to install GHC and Cabal. GHCup will list recommended versions (the latest version that most of the ecosystem is compatible with) and latest versions.

## Setting up your editor

Writing Haskell without good IDE support is a pretty annoying experience:

- For VS Code, you’ll want to install and use the [official Haskell plugin](https://www.haskell.org/ghcup/).
- For Emacs, you’ll want to install and use [haskell-mode](https://github.com/haskell/haskell-mode).

Other editors might have good Haskell support, but their plugins are not officially supported by the Haskell.org committee. In a pinch (if you can’t get any other plugins working properly), use [ghcid](https://github.com/ndmitchell/ghcid), which is a simple and dumb daemon that just reloads `ghci` when file changes are detected.

Expect a good editor plugin to give you:

- Type information of expressions on hover. This is the killer feature. It is extremely useful for debugging, and writing Haskell without this functionality is much more annoying.
- Type-driven autocomplete for holes.
- Inline compilation errors and warnings.
- Automatic symbol autocomplete and imports management.
- Symbol renaming.

# Writing your first Haskell program

## Writing quick scripts

Now that GHC is installed, let’s get started by compiling a working program. Don’t worry about understanding the code right now. Our goal right now is to get familiar with the compiler as a tool.

Let’s start with this "Hello, World" program (see [exercise 001-hello-world](./exercises/001-hello-world/)):

```hs
module Main (main) where

main :: IO ()
main = putStrLn "Hello, World!"
```

Save this file as `hello.hs`. With a simple script like this, we have a couple options for execution.

- `ghc hello.hs` will produce an executable `hello`, which you can run using `./hello`.
- `runghc hello.hs` will interpret this program.

We can also load the program into a REPL using `ghci hello.hs`. Once loaded, evaluate `main` to execute the program.

## `ghc` versus `cabal`

Writing quick scripts like this can be useful for small, one-off programs. However, most of the time you'll want to set up a properly built project using `cabal`.

Why? Because `ghc` is a compiler, not a build tool. Once you start building programs that bring in other modules (e.g. by importing dependencies), you'll need to manually configure `ghc`'s flags so it knows where to look for the code for those modules. This quickly becomes an annoying, tedious, unmanageable mess.

`cabal` handles invoking `ghc` for us. All we need to do is set up a project in a structure that `cabal` understands, and it will handle the rest. If you're familiar with other compiled languages, some analogies here are:

- `ghc` is like `rustc`, while `cabal` is like `cargo`.
- `ghc` is like `javac`, while `cabal` is like `mvn`.

<!-- ## Setting up a proper Cabal project -->

<!--

# Learning the language

## Before we start, tips and tricks

- Bidirectional type inference
- Minimal syntax
- Parentheses
- Separate namespaces
- Language pragmas
- Compiler errors and warnings
- Using libraries that do fancy stuff you don’t understand
- Approaching libraries in general
- Type signatures are much stronger in Haskell

## Taking apart Hello, World

- How modules work
  - Modules are in Haskell, packages are in Cabal
  - Modules map to the file system
- Evaluation model (why we need monads)

[ Maybe do this before tips and tricks? Do tips and tricks when we start _writing_. ]

## Weird new things

- ADTs
- Data constructors vs. Type constructors
- newtype vs. type synonyms

## Analogies

- Typeclasses and interfaces

-->
