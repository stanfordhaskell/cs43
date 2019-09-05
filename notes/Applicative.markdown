---
title: Applicative
---

## Intuition

`Applicative` is the class for _applicative functors_, an intermediate class between `Functor` and `Monad`. (add details on applicative style).

## Definition

```haskell
class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

The method `pure` encapsulates values into the functor `f`.  `<*>` allows us to apply a function in a computational context.

The Maybe instance looks like this:
```haskell
instance Applicative Maybe where
    pure                  = Just
    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing
```

(Motivate the hierarchy from functor -> applicative -> monad.)

