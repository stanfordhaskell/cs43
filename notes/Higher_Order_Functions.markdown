---
title: Higher order functions
published: 2000-01-03
---

---
title: Introduction to Haskell
---

## List-based recursion

Consider the `length` function that finds the length of a list.  Below is a
recursive definition.

```haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```

The type signature tells us that `length` is a function that accepts a list of
an arbitrary type `a`, and returns an `Int`. The `(x:xs)` term binds `x` to the
head of the list and `xs` to the tail.

```haskell
ghci> head [1,2,3]
1
ghci> tail [1,2,3]
[2,3]
```

This works by pattern matching on the type constructor `(:)`. If we had
recursively defined our list as 

```haskell
data List a = Nil
            | Cons a (List a)
```

then the same length function would be written as follows.

```haskell
length :: List a -> Int
length Nil = 0
length (Cons x xs) = 1 + length xs
```

## Abstracting to map

Suppose we're writing a function that doubles every element in a list of
`Int`s.  We might write the following.

```haskell
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = (2 * x) : doubleList xs
```

We can easily generalize this, for example to a function that multiplies every
element in a list of `Int`s by $m$.

```haskell
multiplyList :: Int -> [Int] -> [Int]
multiplyList _ [] = []
multiplyList m (x:xs) = (m * x) : multiplyList m xs
```

Here, the naive reading of the type signature is as a function from an `Int` and
a `[Int]` to a `[Int]`. However, since the `->` operator is right associative,
we can read the type signature as

```haskell
multiplyList :: Int -> ([Int] -> [Int])
```

meaning `multiplyList` takes an `Int` and returns a function from `[Int]` to
`[Int]`. In particular, note that we can recover `doubleList` by passing one
argument to `multiplyList`.  The following code gives us a function equivalent
to the original.

```haskell
doubleList' = multiplyList 2
```

Functions in Haskell are "first-class citizens," and behave exactly like any
other value.  As we've seen, functions can return other functions.  Similarly,
functions can accept other functions as arguments. To generalize `multiplyList`
further, we can write

```haskell
applyToInts :: (Int -> Int) -> [Int] -> [Int]
applyToInts _ [] = []
applyToInts f (x:xs) = (f x) : applyToInts f xs
```

`applyToInts` accepts a function from integers to integers, with type signature
`Int -> Int`, and returns a function that applies that function to each element
in a list. <!-- Note it isn't actually easy to recover multiplyList, the
solution listed earlier `multiplyList = applyToInts` is incorrect. -->

```haskell
ghci> applyToInts (* 2) [1,2,3]
[2,4,6]
```

We can further generalize `applyToIntegers`.  While its type signature is `(Int
-> Int) -> [Int] -> [Int]`, the definition is not integer specific. We can make
a polymorphic version with the type signature `(a -> b) -> [a] -> [b]`.  This
function is called `map`, which you might be familiar with.

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x) : map f xs
```

## The filter function

Besides `map`, Haskell contains numerous other higher-order functions for list
operations. As in other languages, `filter` is a function that takes a predicate
and a list, and returns the list of elements that satisfy the predicate.

```haskell
filter :: (a -> Bool) -> [a] -> [a],
filter _ [] = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
```

Looking at the type signature, we can see that the predicate, which is the first
argument, has type `(a -> Bool)` where `a` is the type of the list elements.

Here are some examples.

```haskell
ghci> filter even [1..10]
[2,4,6,8,10]
ghci> filter (>2) [1,6,2,3,8,3]
[6,3,8,3]
```

As with `map`, we can define more specific functions using filter. For example,
the following function `justEvens` takes a list of type `[Int]` and returns a
list of its even elements.

```haskell
justEvens :: [Int] -> [Int]
justEvens = filter even
```

