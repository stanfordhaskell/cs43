---
title: Functor and Applicative
---

Intro stuff.

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
 
  (<$) :: a        -> f b -> f a
  (<$) = fmap . const
```

 
## Applicative<label for="applicative" class="margin-toggle sidenote-number"></label>

<input type="checkbox" id="applicative" class="margin-toggle"/>
<span class="sidenote">
    Applicative on the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Applicative).
</span>
