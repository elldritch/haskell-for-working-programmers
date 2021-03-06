# Haskell for Working Programmers <!-- omit in toc -->

_Haskell for Working Programmers_ is a guide for professional programmers for picking up Haskell. Most Haskell learning materials swing too hard in either direction regarding experience: they assume either an academic computer science background, or clean start from complete basics.

This guide is intended for folks working a working, professional knowledge of an existing popular programming language. We’ll skim through common concepts shared with other languages (e.g. "what is a string?"), and learn Haskell-specific concepts by comparing, contrasting, and drawing analogies to other more common languages.

We’ll start by getting your machine set up to build Haskell programs. Then, we’ll compile a program end-to-end and get a "Hello, World" program working. Afterwards, we’ll run through a crash course of Haskell programming concepts. Finally, we’ll put those concepts into practice by building some non-trivial real-world programs.

# Table of Contents <!-- omit in toc -->

<!-- Generated by https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one -->

- [Getting set up](#getting-set-up)
  - [Setting up build tools](#setting-up-build-tools)
  - [Setting up your editor](#setting-up-your-editor)
- [Writing your first Haskell program](#writing-your-first-haskell-program)
  - [Writing quick scripts](#writing-quick-scripts)
  - [`ghc` versus `cabal`](#ghc-versus-cabal)
  - [Setting up a proper Cabal project](#setting-up-a-proper-cabal-project)
    - [TL;DR](#tldr)
    - [The `.cabal` file](#the-cabal-file)
    - [Haskell files, modules, and imports](#haskell-files-modules-and-imports)
    - [Compiling and running the project](#compiling-and-running-the-project)
- [Learning the language](#learning-the-language)
  - [Re-examining "Hello, World"](#re-examining-hello-world)

# Getting set up

## Setting up build tools

Use [ghcup](https://www.haskell.org/ghcup/) to install and manage versions of GHC, Cabal, Stack, and HLS. GHC is the main Haskell compiler, and Cabal is the main build tool. If you’re familiar with Node.js, some analogies here are:

- GHC ~= Node. It’s not the only Haskell compiler (much like how Node isn’t the only standalone JS runtime), but it’s the one everyone uses.
- Cabal ~= NPM. It’s the build tool most people use, and is the official one.
- Stack ~= Yarn. It’s an alternative build tool to Cabal, and it was a lot better back than Cabal back before Cabal natively supported sandboxes. It does mostly the same things, but used to handle dependencies better. Nowadays, don’t bother - you should prefer Cabal unless you know what you’re doing and deliberately need a Stack-specific feature.

HLS is the Haskell Language Server. You usually don’t need to install this standalone, because the most popular code editors’ Haskell plugins usually manage this for you.

You’ll want to install GHC and Cabal. GHCup will list recommended versions (the latest version that most of the ecosystem is compatible with - this is sometimes not the latest version when breaking changes to the standard library are made) to install.

## Setting up your editor

Writing Haskell without good IDE support is a pretty annoying experience. You should use an officially supported editor:

- For VS Code, you’ll want to install and use the [official Haskell plugin](https://www.haskell.org/ghcup/).
- For Emacs, you’ll want to install and use [haskell-mode](https://github.com/haskell/haskell-mode).

Other editors might have good Haskell support, but their plugins are not officially supported by the Haskell.org committee. In a pinch (if you can’t get any other plugins working properly), use [ghcid](https://github.com/ndmitchell/ghcid), which is a simple and dumb daemon that just reloads `ghci` (the Haskell REPL) when file changes are detected.

Expect a good editor plugin to give you:

- Type information of expressions on hover. This is the killer feature. It is extremely useful for debugging, and writing Haskell without this functionality is much more annoying.
- Type-driven autocomplete for holes.
- Inline compilation errors and warnings.
- Automatic symbol autocomplete and imports management.
- Symbol renaming.

# Writing your first Haskell program

## Writing quick scripts

Now that GHC is installed, let’s get started by compiling a working program. Don’t worry about understanding the code right now. Our current goal is to get familiar with the compiler as a tool.

Let’s start with this "Hello, World" program (see [exercise 001-hello-world-script](./exercises/001-hello-world-script/)):

```hs
module Main (main) where

main :: IO ()
main = putStrLn "Hello, World!"
```

Save this file as `hello.hs`. With a simple script like this, we have a couple options for execution:

- `ghc hello.hs` will produce an executable `hello`, which you can run using `./hello`.
- `runghc hello.hs` will interpret this program.

We can also load the program into a REPL using `ghci hello.hs`. Once loaded, evaluate `main` to execute the program.

## `ghc` versus `cabal`

Writing quick scripts like this can be useful for small, one-off programs. However, most of the time you'll want to set up a properly built project using `cabal`.

Why? Because `ghc` is a compiler, not a build tool. Once you start building programs that bring in other modules (e.g. by importing dependencies), you'll need to manually configure `ghc`'s flags so it knows where to look for the code for those modules. This quickly becomes an annoying, tedious, and unmanageable mess.

`cabal` handles invoking `ghc` for us. All we need to do is set up a project in a structure that `cabal` understands, and it will handle the rest. If you're familiar with other compiled languages, some analogies here are:

- `ghc` is like `rustc`, while `cabal` is like `cargo`.
- `ghc` is like `javac`, while `cabal` is like `mvn`.

## Setting up a proper Cabal project

Let's get "Hello, World" set up into a proper project. Take a look at [exercise 002-hello-world-project](./exercises/002-hello-world-project/). We're going to go through this project line-by-line.

### TL;DR

If you want to jump directly into the code, feel free to skip this section and come back if you're confused. The most important thing we explain here is how the module system (imports, exports, and filesystem layout) works. But all you really need to know to start touching code is that `cabal build` builds the project, `cabal run hello` will run the executable in this exercise, and `cabal test` will run the tests.

### The `.cabal` file

Let's start by examining [`hello-world-project.cabal`](exercises/002-hello-world-project/hello-world-project.cabal), which defines the Cabal project. You can find documentation for all of these fields [in the `cabal` docs](https://cabal.readthedocs.io/en/3.6/cabal-package.html#package-properties).

```cabal
cabal-version: 3.0
name:          hello-world-project
version:       0.1.0.0
```

We start off with the usual front matter. The `cabal-version` here is the version of the `.cabal` file, _not_ the version of the `cabal` executable that you're using. The supported file versions of each executable are listed [in the `cabal` docs](https://cabal.readthedocs.io/en/3.6/cabal-package.html#pkg-field-cabal-version).

The `name` and `version` fields describe the project:

- Notice that the `name` of the project matches the file name of the `.cabal` file. This naming is a convention, but is not required.
- Notice that the `version` string has _four_ sections instead of three. This is because `.cabal` files (and the Haskell package ecosystem in general) use Haskell's [Package Versioning Policy](https://pvp.haskell.org/) specification for versions, which is different from the commonly used [SemVer](https://semver.org/) specification. The main difference is that the sections are `major.major.minor.patch` rather than `major.minor.patch`.

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

Besides top-level project settings, a `.cabal` file defines a list of _sections_. Some of these sections define build targets (e.g. `library`, `executable`, `test-suite`, `benchmark`, etc.), which Cabal calls _components_.

This section is a [common stanza](https://cabal.readthedocs.io/en/3.6/cabal-package.html#common-stanzas) named `lang`, which lets you factor out common shared fields for other sections. In this one, we define some dependencies shared by every section, as well as some shared compiler options.

```cabal
library
  import:          lang
  hs-source-dirs:  src

  -- cabal-fmt: expand src
  exposed-modules: HFWP.SomeLibrary
```

This section is a [library section](https://cabal.readthedocs.io/en/3.6/cabal-package.html#library). Libraries contain the bulk of your code. If you decide to publish this project as a [package](https://cabal.readthedocs.io/en/3.6/developing-packages.html#package-concepts) on Hackage, the code that other users will be able to consume is the code contained in your `library` section.

There are a couple of important fields in this section:

- `import: lang` imports the fields defined in common stanza named `lang` defined above into this section.
- `hs-source-dirs: src` tells Cabal to look in the `src` folder (relative to this `.cabal` file) for Haskell modules that belong to this section. In particular, this means that the module names of modules in this section will be relative to the `src` folder.
  - For `library` sections, I usually use `src` as the source directory name.
  - We'll talk more about how modules work in a bit once we start looking at the Haskell files themselves.
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

This section is an [executable section](https://cabal.readthedocs.io/en/3.6/cabal-package.html#executables) named `hello`. Executable sections define the binaries that get produced when we `cabal build` this project. Each binary has its own section, and the compiled binary will be named whatever its corresponding executable section is named. In this case, this section defines the entrypoint for a binary named `hello`.

Code in executable sections should be a very thin wrapper over library code. For example, you might handle CLI flag parsing or other startup/shutdown logic here while importing the vast majority of your business logic from your `library`.

In this section:

- `import: lang` imports `lang` like how it was imported in the `library` section.
- `hs-source-dirs: cmd/hello` defines the root directory that modules in this section are located in.
  - For executables, I like to steal the Go convention of using `cmd/FOO` for programs named `FOO`. It's a useful way to keep binaries together while also giving them each their own file tree.
- `main-is: Main.hs` defines the main module for this binary. The file path to this module is relative to the `hs-source-dir` of the section. Each executable must have exactly one main module, which is a module named `Main` that exports a value named `main` of type `IO ()`. The entrypoint of the binary is evaluating `main`.
  - We'll talk about the execution model later when we start talking about the language.
  - It's convention to name this file `Main.hs` since it contains a module named `Main`, but that isn't strictly required. We'll talk about modules and file names in a bit when we start looking at the Haskell files themselves.
- `other-modules` behaves like `exposed-modules` in `library` sections. It defines a list of other Haskell modules within this section's `hs-source-dirs` that are visible to the `Main` module. Usually, this is used for refactoring more complicated binaries into separate files.
  - Like in `exposed-modules`, we use `cabal-fmt` here to automatically populate this list.
- `build-depends: hello-world-project` defines a list of `library` dependencies that this component depends on. In this case, we're declaring that this component depends on exactly one library named `hello-world-project` at any version. This is actually the `library` provided by our own project.
  - Depending on your own library without version constraints is the common way to make your library code visible to your other components.
  - Note that you can also create executables that _don't_ include your library code. You might rarely want to do this to reduce the binary size of one-off tools.

All executables can be compiled and run using `cabal run COMPONENT`. For example, you can run this executable using `cabal run hello`. If you have other components of different type that are also named `hello`, you can use this component's fully-qualified component name with `cabal run exe:hello`.

```cabal
test-suite tests
  import:         lang
  type:           exitcode-stdio-1.0
  hs-source-dirs: test
  main-is:        Main.hs

  -- cabal-fmt: expand test -Main
  other-modules:
  build-depends:  hspec ^>=2.9.4 -- TODO: also import library and add a test
```

Finally, this section is a [test suite section](https://cabal.readthedocs.io/en/3.6/cabal-package.html?highlight=build-depends#test-suites) named `tests`.

Notice that this section is roughly the same as the `executable` with two differences:

- `type: exitcode-stdio-1.0` indicates the type of this test. You almost always want this value to be `exitcode-stdio-1.0`. In this mode, the test suite is treated as a special kind of executable. When it runs, it signals success by exiting 0 and failure otherwise.
- `build-depends: hspec ^>=2.9.4` shows an example of loading an external dependency from Hackage. In this case, we're loading the [`hspec`](https://hackage.haskell.org/package/hspec) package (which is useful for writing tests) at the latest version within the [version spec](https://cabal.readthedocs.io/en/3.6/cabal-package.html#pkg-field-build-depends) `^>=2.9.4` that's compatible with the rest of this component's build.

### Modules, files, and imports

Now that we've seen how the project is laid out, let's look at how the actual individual Haskell modules interact.

Haskell's language specification ([spec](https://www.haskell.org/onlinereport/modules.html), [tutorial](https://www.haskell.org/tutorial/modules.html)) defines a notion of "modules", which act as namespaces of symbols. Module names can be any valid Haskell identifier that begins with a capital letter.

GHC (the compiler) implements modules by mapping every module to a file whose name matches the module name after replacing dots with directory separators ([spec](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html)). For example, module `A.B.C` should be defined in file `A/B/C.hs`.

Cabal (the build tool) handles making sure that GHC's search paths are set correctly for each component in the Cabal project.

Our example project has three Haskell modules:

1. `Main` in section `executable hello`.
2. `HFWP.SomeLibrary` in section `library`.
3. `Main` in section `test-suite tests`.

Notice how the module's file locations match their module names.

To import a symbol `s` into module `A` from module `B`:

1. Make sure that `B` is visible to `A` (e.g. `B` is in the same component as `A`, or `B` is in a component or package that is a `build-depends` for `A`). You'll want to configure this in your `.cabal` file.
2. In `B`, make sure your module exports `s` (e.g. `module B (s) where`).
3. In `A`, import `s` (e.g. `import B (s)`).

Notice that module names can overlap between different components or packages! If you're trying to import a module whose name conflicts with an existing module, GHC and Cabal provide some tricks (e.g. [`PackageImports` pragma](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/package_qualified_imports.html), [mixins](https://cabal.readthedocs.io/en/3.6/cabal-package.html#pkg-field-mixins), see [Stack Overflow](https://stackoverflow.com/questions/47110907/what-should-i-do-if-two-modules-share-the-same-name)) to disambiguate or rename modules.

Ideally, avoid naming your modules so that they can collide. For applications that I'm writing, I usually namespace my modules by application name.

### Importing third-party dependencies from Hackage

In addition to modules, Cabal adds a notion of "packages", which are units of distribution of code on Hackage. Your `.cabal` file defined a single package.

You can `build-depends` on a package by name to tell Cabal to download that package and make the modules it contains visible to the component declaring the dependency.

If you're familiar with Node.js, a Cabal package is roughly an NPM package, and a Haskell module is roughly single Node.js file. Fun fact: individual files in Node are actually also [their own separate modules](https://nodejs.org/api/modules.html#modules-commonjs-modules) (they are individual CommonJS modules)!

### Compiling and running the project

With your project set up properly, Cabal provides some useful commands:

- `cabal build [COMPONENT]` runs an incremental rebuild of `COMPONENT`, and shoves all of its build state into `./dist-newstyle` locally. You'll want to `.gitignore` this folder.
- `cabal run [EXECUTABLE_COMPONENT]` incrementally rebuilds `EXECUTABLE_COMPONENT` and then executes it.
- `cabal list-bin EXECUTABLE_COMPONENT` outputs the location of a built binary.
- `cabal test [TEST_COMPONENT]` incrementally rebuilds `TEST_COMPONENT` and runs the test suite.

# Learning the language

Alright, now that we've properly set a project up and learned about how it's laid out, let's look at the actual program. We'll start by examining the syntax and semantics of our existing "Hello, World" program. Afterwards, we'll start adding to this program, implementing new features, and learning about new language features as we go.

## Re-examining "Hello, World"

We'll start by doing a line-by-line walkthrough of [`cmd/hello/Main.hs`](./exercises/002-hello-world-project/cmd/hello/Main.hs) in [exercise 002-hello-world-project](./exercises/002-hello-world-project).

```hs
module Main (main) where

main :: IO ()
main = putStrLn "Hello, world!"
```

- main
- prelude
- values and signatures
- evaluation model

<!-- then let's add complexity to hello world to introduce ADTs and typeclasses (maybe Show typeclass first?) -->

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
- Language pragmas

## Analogies

- Typeclasses and interfaces

## Using other libraries

- Packages vs. modules
- Hackage
- Publishing code
- Consuming code

-->
