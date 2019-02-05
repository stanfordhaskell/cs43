---
title: Functor
---

## Functor<label for="functor" class="margin-toggle sidenote-number"></label>

<input type="checkbox" id="functor" class="margin-toggle"/>
<span class="sidenote">
    Functor on the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Functor).
</span>

The `Functor` is the most fundamental typeclass in the standard libraries.  Intuitively, a `Functor` can be viewed as a sort of "container," coupled with an ability to apply a function to every element in the container.  One example is a list: this is a container of elements, and we can uniformly apply a function to every element using `map`.

## Definition

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

  -- Replace all locations in the input with
  -- the same value.  This may be
  -- overridden with a more efficient version.
  (<$) :: a        -> f b -> f a
  (<$) = fmap . const
```

Having a single `fmap` method means that we don't have to implement and remember several `map` methods for different data structures e.g. `map` over lists, `treeMap`, `maybeMap`.  

This also enables us to write code that works with any `Functor`, simply by invoking `fmap` polymorphically.  This enables a powerful sort of code reuse.

## Intuition

There are two main intuitions for `Functor`.

1) A `Functor` is a container, or more precisely, a _computational context_ we can map over.  Data structures are the most natural example of this.

2) Since `fmap` is curried, we can write the type signature as `fmap :: (a -> b) -> (f a -> f b)`.  It transforms a "normal" function `g :: a -> b` into one that operates over containers `fmap g :: f a -> f b`.  This transformation is called a _lift_.

## The `[]` instance

Recalling the familiar pattern of mapping over a list, we can implement an instance of `Functor` as follows.

```haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap g (x:xs) = g x : fmap g xs
  -- Alternatively, fmap = map works here.
```

As we'd expect, `fmap` works like `map`:

```
ghci> fmap (\x -> x + 2) [1..10]
[3,4,5,6,7,8,9,10,11,12]
ghci> fmap (*2) [1..10]
[2,4,6,8,10,12,14,16,18,20]
```

## The `Maybe` instance

Similarly, `Maybe` is an instance of functor:

```haskell
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just a) = Just (g a)
```

## The `Tree` instance

Suppose we have a `Tree` data structure defined recursively as follows:

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
```

We can write a `Functor` instance as follows:

```haskell
instance Functor Tree where
    fmap f (Leaf x)            = Leaf   (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
```

This gives us a function that operates as follows:

```haskell
*Main> x
Branch (Leaf 1) (Leaf 2)
*Main> :t x
x :: Num a => Tree a
*Main> fmap (+2) x
Branch (Leaf 3) (Leaf 4)
```

## Laws

There are two laws any `Functor` instance must satisfy:

1) `fmap id = id`

This just means mapping `id` over a container must leave the container unchanged.

2) `fmap (g . f) = fmap g . fmap f`

This says that it doesn't matter whether we map a composed function or first map one and then the other.
 
## Applicative<label for="applicative" class="margin-toggle sidenote-number"></label>

<input type="checkbox" id="applicative" class="margin-toggle"/>
<span class="sidenote">
    Applicative on the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Applicative).
</span>

## Further references

- https://en.wikibooks.org/wiki/Haskell/The_Functor_class
