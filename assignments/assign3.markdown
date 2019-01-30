The `Functor` is the most fundamental typeclass in the standard libraries.  Intuitively, a `Functor` can be viewed as a sort of "container," coupled with an ability to apply a function to every element in the container.

It is defined as follows, where the `fmap` function generalizes the `map` function.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

1. The `Either` type represents values with two possibilities: a value of type `Either a b` is either `Left a` or `Right b`.  
 

```haskell
data Either a b = Left a | Right b
```

By convention, the `Left` constructor is used to hold an error value and the `Right` constructor is used to hold a correct value.

Define an instance of `Functor` for `Either e` for a fixed `e`.

2. The `Data.Tree` module implements a rose tree - also, a multiway tree - as follows.

```haskell
data Tree a = Node a [Tree a]
```

Define an instance of `Functor` for this datatype.

3. Is this statement true or false?

The composition of two `Functor`s is also a `Functor`.

If false, give a counterexample; if true, provide an explanation (ideally with some code).