# Writing your first program

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

Hooray! You now have a working Haskell program.

## `ghc` versus `cabal`

Writing quick scripts like this can be useful for small, one-off programs. However, most of the time you'll want to set up a properly built project using `cabal`.

Why? Because `ghc` is a compiler, not a build tool. Once you start building programs that bring in other modules (e.g. by importing dependencies), you'll need to manually configure `ghc`'s flags so it knows where to look for the code for those modules. This quickly becomes an annoying, tedious, and unmanageable mess.

`cabal` handles invoking `ghc` for us. All we need to do is set up a project in a structure that `cabal` understands, and it will handle the rest. If you're familiar with other compiled languages, some analogies here are:

- `ghc` is like `rustc`, while `cabal` is like `cargo`.
- `ghc` is like `javac`, while `cabal` is like `mvn`.

## Setting up a proper Cabal project

Let's get "Hello, World" set up into a proper project. Take a look at [exercise 002-hello-world-project](./exercises/002-hello-world-project/) and copy it. Take a quick glance at `hello-world-project.cabal`, but don't worry too much about how it works (we'll come back to it later). For now, here are the things you need to know:

- `cabal build` builds your code.
- `cabal run` runs a built executable, defined by an `executable` section in your Cabal file.
- `cabal test` runs a test suite, defined by a `test-suite` section in your Cabal file.

For now, the command you'll be using most is `cabal run`. Run it now, and check that your build succeeds.

If it does, then congratulations! It's now time to draw the rest of the owl.
