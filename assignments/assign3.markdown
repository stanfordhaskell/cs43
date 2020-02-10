---
title: Assignment 3
---

This assignment introduces two new constructs / GHC extensions, and uses them
along with a monadic extension to the parsing library we saw in class to
implement a simple type-checked calculator-style language, with a parser that
parses untyped expressions into our typed language. It requires only a small
amount of new code to be written, but reading through all the code carefully and
working out the types of various expressions in your head / in ghci will be
important both to complete the code without too much trial and error and
to get the most out of the assignment.

## Set Up

Download the starter code from the [repo](https://github.com/ischeinfeld/cs43-assignments). 
The `assignment3` folder is a stack project with the following directory structure.

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── assignment3.cabal
├── src
│   ├── Interpreter.hs
│   ├── Language.hs
│   ├── Parser.hs
│   └── Problem1.hs
├── stack.yaml
└── stack.yaml.lock
```

The problems below should be solved by editing the corresponding files in
`/src/`. It is configured (in the file `assignment3.cabal`) so that

```
$ stack build
```

builds an executable called `interpret`, which can be run as follows.

```
$ stack exec interpret
```

This will present a prompt and evaluate each entered expression. Before
starting, it will simply crash as we have `undefined` expressions remaining.

```
$ stack exec interpret
> 1
interpret: Prelude.undefined
CallStack (from HasCallStack):
  error, ...
```

Once the assignment is completed, you should be able to replicate this session.

```
$ stack exec interpret
> (+ 1 2)
3
> a
Invalid Expression
> (& True False)
False
> (if (& True False) then 1 else 2)
2
> (if (& True False) then 1 else False)
Invalid Expression
```

Note how the last expression is rejected because `1` and `False` are terms with
different types, and therefore the expression fails to parse.

## Problems

### Problem 1 - Getting to know existential quantification

To begin, read
[this](https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types)
description of existentially-quantified types in Haskell, up to and not
including "A Further Explanation." 

This problem is intended to get you comfortable using existential
quantification. Complete the starter code in `/src/Problem1.hs`.

### Problem 2 - Getting to know GADTs

To begin, read
[this](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts)
description of GADTs in Haskell.

In this problem you complete the implementation of a typed 
representation for a simple calculator-like language. Complete the
starter code in `/src/Language.hs`. The starter code describes the syntax
of the language. The semantics are just the obvious ones.

### Problem 3 - Monadic parsing

In class, we saw how you can build a parser library around the interface
provided by the Applicative typeclass. It should not be surprising that you can
write a Monad instance for the same Parser type, and doing so actually allows
you to write more powerful parsers. Specifically, the specialized signature of
`(>>=) :: Parser a -> (a -> Parser b) -> Parser b` hints at the fact that what
is parsed after `Parser a` is run can depend on the value of what is parsed.
This allows us to effectively parse context-sensitive grammars, where our
Applicative interface most naturally allowed context-free grammars. (It is
sometimes stated that the Applicative interface *cannot* parse context-sensitive
languages. This is not true; see
[this](https://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/)
blog post if you are interested.) Also, this gives us access to do syntax,
allowing code like the following:

```haskell
token :: Parser a -> Parser a
token p = do
  spaces
  a <- p
  spaces
  return a
```

Complete the starter code in `/src/Parser.hs`.

### Problem 4 - A simple language interpreter

Finally, we can bring everything together. Complete the starter code in
`/src/Interpreter.hs` and test out the interpreter. Optionally, extend it and
tell us what you did!

## Submission instructions

Send an email to cs43-win1920-staff@lists.stanford.edu with a .zip file with your code.
