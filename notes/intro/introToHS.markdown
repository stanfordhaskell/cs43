---
title: Introduction to Haskell
---

Haskell doesn't really have loops like imperative languages.  So, we need to rely on recursion to execute standard programming tasks.  We'll start with some easy examples since it takes a while to get used to thinking functionally.

In Haskell, we can define a factorial function as follows:

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

The first line defines the base case.  The second line defines the recursive step.

\todo{Add a quick note on tail call recursion}

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

## Concatenation

The `(++)` function denotes concatenation, and joints two lists together.  

```haskell
Prelude> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]
```

Think about this definition, and try to parse what this code means.

```haskell
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
```
