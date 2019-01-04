---
title: Introduction to Haskell
---

Haskell doesn't really have loops like imperative languages.  So, we need to rely on recursion to execute standard programming tasks.  We'll start with some easy examples since it takes a while to get used to thinking functionally.

## List-based recursion

Consider the `length` function that finds the length of a list.  Below is a recursive definition.

```haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```

The first line tells us the type signature.  `length` is a function that accepts a list of an arbitrary type `a`, and returns an `Int`.  The `x:xs` section is a feature of list-based pattern matching.  Here's how the function works:

- Check if the list is `[]`.  If so, return 0.
- If not, bind the variable `x` to the head of the list, and `xs` to the tail.
- Recurse on \T{xs}.



## Introduction to `map`

[TODO: figure on stacked abstractions]

Suppose we're writing a function that doubles every element in a list of `Integer`s.  We might write a function as follows:

```haskell
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = (2 * x) : doubleList xs
```

The first line is a type signature that states that "`doubleList` is a function that accepts lists of `Integer`s and returns lists of `Integer`s."  The next few parts specify the base case and recursive case.

We can easily generalize this.

We might write a function that multiplies every element in a list of `Integer`s by $m$.

```haskell
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (x:xs) = (m * x) : multiplyList m xs
```

The type signature looks different here.  It turns out that the `->` operator is right associative, so we can read this like

```haskell
multiplyList :: Integer -> ([Integer] -> [Integer])`
```

Intuitively, `multiplyList` takes a single integer and then returns a function with type signature `[Integer] -> [Integer]`.  In Haskell, this is _technically true_.  In particular, note that we can recover `doubleList` by passing one argument to `multiplyList`.  The following code

```haskell
doubleList' = multiplyList 2
```
gives us a function equivalent to the original.

This is important.  Functions in Haskell are "first-class citizens," and behave like any other value.  As we've seen, functions can return other functions.  We now ask: can we accept functions _as arguments_?  The short answer is yes.  To generalize `multiplyList` further, we can write

```
applyToIntegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToIntegers _ [] = []
applyToIntegers f (x:xs) = (f x) : applyToIntegers f xs
```

Now, we can recover multiplyList as follows:

```
multiplyList = applyToIntegers *
```

The take home message here is that _all Haskell functions only take one argument_.  This process, of creating intermediate functions when passing arguments into a complex function is called _currying_.

Can we generalize `applyToIntegers` further still?  Indeed we can.  While its type signature is `(Integer -> Integer) -> [Integer] -> [Integer]`, the definition is not integer specific.  We can make a polymorphic version with the type signature `(a -> b) -> [a] -> [b]`.  This function is called `map`, which you might be familiar with:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x) : map f xs
```


-- Source: Haskell wikibook.