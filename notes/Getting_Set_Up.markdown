---
title: Getting Set Up
---

## Haskell

The simplest way to get started with Haskell is the
[Stack](https://docs.haskellstack.org/en/stable/README/) build system. Stack
handles sandboxing (allowing different projects to use different versions of the
GHC (TODO note) compiler and packages), dependency resolution, and building Haskell
applications.

### Installation

On Mac, you can install Stack using the [Homebrew](https://brew.sh) package
manager.

```
brew install stack
```

For other operating systems or if you do not want to use Homebrew, see the
[installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

### Starting a New Project

The following command will create a new project called `first-project` using the `simple`
template

```
stack new first-project simple
```

with the following directory structure.

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── first-project.cabal
├── src
│   └── Main.hs
└── stack.yaml
```

Now, the project can be built into an executable and run.

```
stack build
stack exec first-project
```

For details on Stack, see the [user
manual](https://docs.haskellstack.org/en/stable/GUIDE/#hello-world-example).

### The REPL

An important tool in Haskell development is `ghci`, a REPL (TODO note) provided
by GHC. We can start `ghci` and load our project's code as follows.

```
stack ghci
```

The only function currently defined in our project (in `src/Main.hs`) is `main`, which we
can run in the repl.

```
*Main> main
hello world
```

We can run Haskell code in `ghci`. It is important to note that `ghci` does not
execute code in the same way it is executed in a Haskell program. Instead, it is
executed in a computational context known as a monad, which allows symbols to be
redefined to take on new values, among other things. This will become clearer in
the next set of notes.

`ghci` also supports a variety of specialty commands. The most useful will be
`:r`, which reloads the project code, and `:t x`, which gives the type of `x`.

```
*Main> :r
Ok, one module loaded.
*Main> :t "Hello"
"Hello" :: [Char]
```

## Tooling

A variety of tooling is available for Haskell. We recommend, at least at first,
setting up syntax highlighting in the text editor of your choice and relying
only on it and ghci.

<!---
TODO add links to syntax highlighting and further config
-->
