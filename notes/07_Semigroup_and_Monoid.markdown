---
title: Semigroup and Monoid
---

Intro stuff.

## Semigroup<label for="semigroup" class="margin-toggle sidenote-number"></label>

<input type="checkbox" id="semigroup" class="margin-toggle"/>
<span class="sidenote">
    Semigroup on the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Semigroup).
</span>


Motivation.

### Definition

Some text

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
 
  sconcat :: NonEmpty a -> a
  sconcat = sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b
 
  stimes :: Integral b => b -> a -> a
  stimes = ...
```

### Laws

Laws and math

### Instances

...

## Monoid<label for="monoid" class="margin-toggle sidenote-number"></label>

<input type="checkbox" id="monoid" class="margin-toggle"/>
<span class="sidenote">
    Monoid on the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Monoid).
</span>


Motivation.

### Definition

Text here.

```haskell
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
 
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
```

### Laws

Laws and math

### Instances

...

## Examples

### Diagrams Library

https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html

### Sorting Combinators

https://www.reddit.com/r/programming/comments/7cf4r/monoids_in_my_programming_language/c06adnx/

### Finger Trees

https://apfelmus.nfshost.com/articles/monoid-fingertree.html

http://andrew.gibiansky.com/blog/haskell/finger-trees/

http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf
