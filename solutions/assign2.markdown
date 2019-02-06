---
title: Assignment 2
---

## Problems

## Problem 1

Integers, like booleans, do not have a natural associative operation.
However, both addition and multiplication are associative operations on
integers. Write `Semigroup` instances for the `Sum` and `Product` types
wrapping `Int` in the starter code, mirroring the implementations of `Any`
and `All`.

_See solution to Problem 3._

## Problem 2

The default given above has $O(n)$ performance, which is less efficient than
the best general implementation.  Using the associative property of `<>`,
provide a more efficient implementation, `stimes'`, with time
complexity $O(\log n)$.

```haskell
stimes' :: (Semigroup a, Integral b) => b -> a -> a
stimes' n x
  | n <= 0    = error "positive multiplier expected"
  | n == 1    = x
  | even n    = stimes' m x <> stimes' m x
  | otherwise = stimes' (m + 1) x <> stimes' m x
  where m = n `div` 2
```

## Problem 3

While $O(\log n)$ is the best possible performance in general, specific
instances can perform even better. Extend your instances for `Sum` and `Product`
with $O(1)$ implementations of `stimes`.

```haskell
instance Semigroup Sum where
  (Sum a) <> (Sum b) = Sum $ a + b
  
  stimes n (Sum x)
    | n <= 0    = error "positive multiplier expected"
    | otherwise = Sum $ x * (fromIntegral n)
  
instance Semigroup Product where
  (Product a) <> (Product b) = Product $ a * b
  
  stimes n (Product x)
    | n <= 0    = error "positive multiplier expected"
    | otherwise = Product $ x ^ (fromIntegral n)
```