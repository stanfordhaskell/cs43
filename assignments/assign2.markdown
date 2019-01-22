---
title: Assignment 2
---

## Problems

So far, we have seen that typeclasses allow us to implement functions (for
example, `(+)` in the `Num` typeclass) separately
for various types.^[This is known as *ad hoc* polymorphism, in contrast to the
*parametric* polymorphism provided by type variables.] Another common
Haskell idiom is to create a typeclass for functions with a specific
structure and then let a value's type determine which function with that
structure to apply.
   
For example, consider the `Semigroup` typeclass (already implemented in Haskell).

```haskell
class Semigroup a where
  (<>) :: a -> a -> a

  stimes :: Integral b => b -> a -> a
  stimes n x   -- not the actual default implementation
    | n <= 0    = error "positive multiplier expected" -- runtime error
    | n = 1     = x
    | otherwise = x <> stimes (n - 1) x

  -- 1 more function defined in terms of <>
```

The primary function provided by the `Semigroup` typeclass is `<>`.  Besides the
type signature `a -> a -> a`, the class definition does not tell us anything
about this function. However, the
[documentation](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Semigroup.html)
tells us that instances of `Semigroup` should satisfy the following associativity law.

```haskell
x <> (y <> z) = (x <> y) <> z
```

Just like an implementation of `(+)` for a type being made an instance of `Num`
should conform to the laws of addition, an implementation of `<>` for a type
being made an instance of `Semigroup` should be associative.

Many types have a natural associative operation. For example, concatenation is
the natural associative operation on lists, and lists have a `Semigroup`
instance.

```haskell
instance Semigroup [a] where
  (<>) = (++)
```

Concatenation is associative since `x ++ (y ++ z) = (x ++ y) ++ z`, and
therefore this instance conforms to the `Semigroup` laws.

Some types, however, do not have a natural associative operation. For example,
*or* and *and* are both associative functions on booleans. Since there is not a
natural associative operation on the `Bool` type, it does not make sense to give
`Bool` a `Semigroup` instance. Instead, we can wrap a boolean value in a new
type for each associative function we want to use. 

```haskell
newtype Any = Any Bool deriving (Show)
newtype All = All Bool deriving (Show)
```

Here, `newtype` is basically equivalent to `data`, but can only be used when
there is one constructor with one field. In return, it incurs no runtime cost
over the wrapped type.[^newtype]

[^newtype]: See [here](https://wiki.haskell.org/Newtype) for
  the subtle differences between `data` and `newtype`.

We now have two new types, `Any` and `All`. We can recover the wrapped boolean
by pattern matching, and we can write separate `Semigroup` instances for each.

```haskell
instance Semigroup Any where
  (Any a) <> (Any b) = Any $ a || b

instance Semigroup All where
  (Any a) <> (Any b) = Any $ a && b
```

Now, `Any` values are combined using *or* and `All` values are combined using
*and*.

```haskell
ghci> (Any False) <> (Any True) <> (Any False)
Any True
ghci> (Any False) <> (Any False) <> (Any False)
Any False
ghci> (All False) <> (All True) <> (All True)
All False
ghci> (All True) <> (All True) <> (All True)
All True
```

1. Integers, like booleans, do not have a natural associative operation.
   However, both addition and multiplication are associative operations on
   integers. Write `Semigroup` instances for the `Sum` and `Product` types
   wrapping `Int` in the starter code, mirroring the implementations of `Any`
   and `All`.

1. The power of using typeclasses to represent operations with a given structure
   becomes apparent when we want to write generic functions using the structures
   they represent. Consider the `stimes` function, which is also implemented in
   the `Semigroup` typeclass (see above). It computes the following.
   
   ```haskell
   stimes n x = x <> x <> ... (n times) ...
   ```
   
   It has a default implementation in terms of `<>`. The default given above has
   $O(n)$ performance, which is less efficient than the best general
   implementation. Using the associative property of `<>`, provide a more
   efficient default implementation, `stimes'`, with time complexity $O(\log n)$.

1. While $O(\log n)$ is the best possible performance in general, specific
   instances can perform even better. Extend your instances for `Sum` and `Product`
   with $O(1)$ implementations of `stimes`.

[Here](https://gitlab.com/stanford-lambda/stanford-lambda.gitlab.io/blob/master/starter-code/assignment2/src/Main.hs) is starter code which you should complete for your solutions.

## Submission instructions

Send an email to cs43-win1819-staff@lists.stanford.edu with either:

- (Preferred) A link to a Gitlab / Github repository with your code.

- A .zip file with your code.
