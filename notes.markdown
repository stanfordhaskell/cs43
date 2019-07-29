---
title: Lecture Notes
subtitle: Functional Programming Abstractions
---

These notes form an introductory course on functional programming using the
Haskell programming language, based on Stanford's CS43. They are intended to
briefly present the necessary syntax and basic concepts (which are covered
elsewhere in much greater depth) and focus on further theoretical exposition and
examples. They are written by Adithya Ganesh and Isaac Scheinfeld, under the
advising of Jerry Cain.  Special thanks to Allan Jiang, co-creator of CS43.

Note that since this is the first year this material is being taught in CS43,
this page will be updated throughout the course with new material.

## Background and Basics

1. [Why Haskell?](notes/Why_Haskell.html): An overview of Haskell's defining
   features, with brief examples.
   
1. [Getting Set Up](notes/Getting_Set_Up.html): One way to set up a Haskell environment with stack, as well
   as some options for minimal tooling.

1. [Introduction to Haskell](notes/Introduction_to_Haskell.html): Expressions, values, and types.

1. [Algebraic Datatypes](notes/Algebraic_Datatypes.html): Defining and using types in Haskell, and the basics of type algebra.

## Functional Abstractions

1. [Higher Order Functions](notes/Higher_Order_Functions.html): Functions that operate on other functions, generalizing basic functions, and defining `map`.

1. [Folds, Scans, and Laziness](notes/Folds_Scans_Laziness.html): Examining the many ways to process lists.


## Type Design Patterns

1. [Typeclasses](notes/Typeclasses.html): Introducing typeclasses and polymorphism.

1. [Typeclasses II, Batteries Included](notes/TypeclassesII.html): More on the built-in typeclasses in Haskell.

## Algebraic Abstractions

1. [Semigroup](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Semigroup.html) and [Monoid](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Monoid):
   Abstracting compositionalstructures.

1. [Functor](notes/Functor.html): Lifting function application to a new level.

1. Monad<!--[Monad](notes/Monad.html)-->: It isn't *that* complicated.

## Beyond Haskell

1. Idris and Dependent Types

<!--
## Contributing

1. [Notes Features](notes/Notes_features.html)
-->
