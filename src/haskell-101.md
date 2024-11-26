# Haskell 101

Let's jump right into it. We're again looking at [exercise 002-hello-world-project](../exercises/002-hello-world-project), starting at `./cmd/hello/Main.hs`:

```haskell
module Main (main) where

main :: IO ()
main = putStrLn "Hello, World!"
```

Let's look at this program line by line.

## Modules

```haskell
module Main (main) where
```

This line declares the current file (which is named `Main.hs`) to be a module named `Main` that exports a symbol named `main`.
