---
title: Assignment 3
---

The `Functor` is the most fundamental typeclass in the standard libraries.  Intuitively, a `Functor` can be viewed as a sort of "container," coupled with an ability to apply a function to every element in the container.

It is defined as follows, where the `fmap` function generalizes the `map` function.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

### Problem 1

 The `Either` type represents values with two possibilities: a value of type `Either a b` is either `Left a` or `Right b`.  By convention, the `Left` constructor is used to hold an error value and the `Right` constructor is used to hold a correct value.

```haskell
data Either a b = Left a | Right b
```

Define an instance of `Functor` for `Either e` for a fixed `e`.

### Problem 2

We can implement a rose tree - also, a multiway tree - as follows. The `Data.Tree` module has an implementation; one possible implementation is as follows:

```haskell
data Tree a = Node a [Tree a]
```

Define an instance of `Functor` for this datatype.

### Problem 3

Is this statement true or false? The composition of two `Functor`s is also a `Functor`.

If false, give a counterexample; if true, provide an explanation.

### Submission instructions

See [here](https://gitlab.com/stanford-lambda/stanford-lambda.gitlab.io/blob/master/starter-code/assignment3/src/Main.hs) for the starter code.

Send an email to cs43-win1819-staff@lists.stanford.edu with either:

- (Preferred) A link to a Gitlab / Github repository with your code.

- A .zip file with your code.
