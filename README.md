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

Let’s start with this "Hello, World" program (see [exercise 001-hello-world-script](./exercises/001-hello-world-script/)):

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

## Setting up a proper Cabal project

Let's get "Hello, World" set up into a proper project. Take a look at [exercise 002-hello-world-project](./exercises/002-hello-world-project/), and let's break down the project.

### TL;DR

If you want to jump directly into the code, feel free to skip this section and come back if you're confused. The most important thing we explain here is how the module system (imports, exports, and filesystem layout) works. But all you really need to know to start touching code is that `cabal build` builds the project, `cabal run hello` will run the executable in this exercise, and `cabal test` will run the tests.

### The `.cabal` file

Let's start by examining [`hello-world-project.cabal`](exercises/002-hello-world-project/hello-world-project.cabal), which defines the Cabal project. You can find documentation for all of these fields [in the `cabal` docs](https://cabal.readthedocs.io/en/3.4/cabal-package.html#package-properties).

```cabal
cabal-version: 3.0
name:          hello-world-project
version:       0.1.0.0
```

We start off with the usual front matter. The `cabal-version` here is the version of the `.cabal` file, _not_ the version of the `cabal` executable that you're using. The supported file versions of each executable are listed [in the `cabal` docs](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-cabal-version).

The `name` and `version` fields describe the project:

- Notice that the `name` of the project matches the file name of the `.cabal` file. This naming is not required, but is a convention.
- Notice that the `version` string has _four_ sections instead of three. This is because `.cabal` files use Haskell's [Package Versioning Policy](https://pvp.haskell.org/) specification, which is slightly different from SemVer. The main difference is that the sections are `major.major.minor.patch` rather than `major.minor.patch`.

```cabal
tested-with:   GHC ==9.0.2
```

This field isn't commonly used, but I find it's a useful way to indicate what GHC version you're using. Unfortunately, `cabal` does not check that you're actually using this version of `ghc`.

```cabal
common lang
  build-depends:    base >=4.12 && <4.16
  default-language: Haskell2010
  ghc-options:
    -Wall -Wincomplete-uni-patterns -Wcompat
    -Wincomplete-record-updates -Wmissing-home-modules
    -Wmissing-export-lists -Wredundant-constraints
```

Besides top-level project settings, the `.cabal` file is divided into a number of _sections_ which define a set of .

This section is a [common stanza](https://cabal.readthedocs.io/en/3.4/cabal-package.html#common-stanzas), which lets you refactor common shared attributes for other sections. In this one, we define some dependencies shared by every section, as well as some shared compiler options.

```cabal
library
  import:          lang
  hs-source-dirs:  src

  -- cabal-fmt: expand src
  exposed-modules: HFWP.SomeLibrary
```

This section is a [library section](https://cabal.readthedocs.io/en/3.4/cabal-package.html#library). Libraries contain the bulk of your code. If you decide to publish this project as a [package](https://cabal.readthedocs.io/en/3.6/developing-packages.html#package-concepts) on Hackage, the code that other users will be able to consume will be your `library` section.

There are a couple of important fields in this section:

- `import: lang` imports the fields defined in `common lang` above into this section.
- `hs-source-dirs: src` tells Cabal to look in the `src` folder (relative to this `.cabal` file) for Haskell modules that belong to this section. In particular, this means that the module names of modules in this section will be relative to the `src` folder. We'll talk more about how modules work in a bit once we get to the Haskell files themselves.
- `exposed-modules: ...` lists all the Haskell modules that this library exposes. Every Haskell file is its own module. Modules that are not explicitly listed in this field are not exposed, which means they aren't visible to other code (i.e. other sections or packages) that imports this library.
- `cabal-fmt: expand src` is a comment used as a formatting directive by [`cabal-fmt`](https://github.com/phadej/cabal-fmt), which is a really convenient autoformatting tool for `.cabal` files. This particular directive automatically adds all Haskell modules within a folder to an `exposed-modules` list.

Note that you can also add names to library sections to create [internal libraries](https://cabal.readthedocs.io/en/3.6/cabal-package.html#sublibs). This is an advanced feature for specific weird use cases, and is probably not what you want.

```cabal
executable hello
  import:         lang
  hs-source-dirs: cmd/hello
  main-is:        Main.hs

  -- cabal-fmt: expand cmd/hello -Main
  other-modules:
  build-depends:  hello-world-project
```

TODO: finish

- notice main-is
- you can run cabal run
- notice cmd/hello source-dir as convention stolen from golang
- notice build-depends section

```cabal
test-suite tests
  import:         lang
  type:           exitcode-stdio-1.0
  hs-source-dirs: test
  main-is:        Main.hs

  -- cabal-fmt: expand test -Main
  other-modules:
  build-depends:  hspec ^>=2.9.4
```

- tests are also executables
- notice build-depends external

### Haskell files, modules, and imports

Now that we've seen how the project is laid out, let's look at how the individual Haskell modules interact.

This project has three Haskell modules: `Main` in section `hello`, `HFWP.SomeLibrary` in the `library` section, and `Main` in section `tests`. Let's first look at how modules are named, and then look at how modules can import other modules.

- module names
- importing other modules
  - overlapping module names
- importing from third-party deps (test hspec)

### Compiling and running the project

- cabal build, run, test
- make a small change
- build, run, test again

<!-- ----- -->
<!--
- Cabal file
  - Sections
    - Executable
    - Library
    - Modules
- cmd/Main.hs
  - Imports
  - If in VS Code, might need to open within exercise folder for HLS to work properly
- src/HFWP/SomeLibrary
  - Modules
    - Default module name is Main (delete `module`)
    - Filesystem layout
    - Module names have dots
      - Modules are strings = Hask
      - Module names have dots = GHC
      - Module names are Filesystem = Cabal
  - Exported symbols
- tests
- Cabal commmands

Now, let's compile and run the project, make some changes, and then recompile to see our changes.

# Learning the language

Now that we have a project and a working build to tinker on, let's learn the language. -->

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
- Prelude

[ Maybe do this before tips and tricks? Do tips and tricks when we start _writing_. ]

## Weird new things

- ADTs
- Data constructors vs. Type constructors
- newtype vs. type synonyms

## Analogies

- Typeclasses and interfaces

## Using other libraries

- Packages vs. modules
- Hackage
- Publishing code
- Consuming code

-->
