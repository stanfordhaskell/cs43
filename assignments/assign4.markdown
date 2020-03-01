---
title: Assignment 4
---

This two-part assignment introduces a new design pattern in one part and a new
monad for truly effectful computations in the other. It requires only a small
amount of new code to be written, but reading through the code carefully and
working out the types of various expressions in your head / in ghci will be
important both to complete the code without too much trial and error and
to get the most out of the assignment.

## Setup

Download the starter code from the [repo](https://github.com/ischeinfeld/cs43-assignments). 

## Problems

### Problem 1

The `assignment4-rio` folder is a stack project with the following directory structure.

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── assignment4-rio.cabal
├── src
│   ├── Main.hs
│   ├── Main1.hs
│   ├── Main2.hs
│   ├── Main3.hs
│   ├── Main4.hs
│   ├── Main5.hs
│   ├── Main6.hs
│   └── RIO.hs
├── stack.yaml
└── stack.yaml.lock
```

This problem should be solved by reading and editing the files `Main1.hs`
through `Main6.hs` in order. These implement a very simple program,
demonstrating a rewrite using the ReaderT design pattern. This is an approach to
writing programs centered around the monad instance for `ReaderT env IO a`. 

### Problem 2 - The ST Monad

Will be published this weekend.

## Submission instructions

Send an email to cs43-win1920-staff@lists.stanford.edu with a .zip file with your code.
