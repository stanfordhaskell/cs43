---
title: Typeclasses
---

## Motivation: Equality testing with `(==)` 

Suppose we define the following `Color` type, and want to implement an equality test for it.

```haskell
data Color = Red | Green | Blue
```

A naive strategy would be to do the following:

```haskell
-- Source: RWH, Ch. 6
colorEq :: Color -> Color -> Bool
colorEq Red Red     = True
colorEq Green Green = True
colorEq Blue Blue   = True
colorEq _    _      = False
```

Now, suppose we want to create an equality test for strings.  We might define a function like this:

```haskell
-- Source: RWH, Ch. 6
stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _ _ = False
```

As you might've noticed, writing a separate function for every equality test is not great.

We really want a function

```haskell
(==) :: a -> a -> Bool
```

that compares two values of the same type. However, a single implementation of
`(==)` could not capture the notion of equality for any possible type `a`, and
would not permit us to extend `(==)` to operate on types that we create. Indeed,
some types (such as functions) might not have a reasonable implementation of 
`(==)` yet the type above allows any type to be substituted for `a`.

## A Typeclass to the Rescue

A typeclass allows us to give different definitions of a function for different
types<label for="java-interfaces"
       class="margin-toggle sidenote-number"></label>.
<span class="sidenote">
This is similar to the notion of <a href="http://tutorials.jenkov.com/java/interfaces.html">interfaces in Java</a>.
</span>
It can be seen as an extensible interface to some set of functions, and solves
exactly the problem presented above. Consider the real type signature of `(==)`,

```haskell
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
```

The only difference between this and our hypothetical type signature above is
`Eq a =>`. Things appearing before `=>` in type signatures are typeclass
constraints. In this case, the constraint says that `a` must belong to the
typeclass `Eq`, meaning that `a` must be a type for which the function `(==)` is
defined. A basic definition of the typeclass `Eq` could be 

```haskell
class Eq a where 
  (==) :: a -> a -> Bool
```

This tells us that for a type `a` to be an instance of `Eq`, we must define a
function `(==)` with type `a -> a -> Bool`. Many primitive Haskell types are already
instances of `Eq`, for example

```haskell
Prelude> 1 == 1
True
Prelude> True == False
False
```

## Built-in Eq typeclass

Indeed, here is the definition of the built-in `Eq` typeclass:

```haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool

  x /= y     = not (x == y)
  x == y     = not (x /= y)
```

Importantly, we only need to implement _one of_ `==` or `/=`, since the compiler can figure out the other function by applying `not`.

Now, suppose we wanted to implement `(==)` for the `Color` type defined above.  We just need to make `Color` an instance of the `Eq` typeclass.  Indeed, we can write

```haskell
instance Eq Color where
    (==) Red Red     = True
    (==) Green Green = True
    (==) Blue Blue   = True
    (==) _    _      = False
```


Similarly, if we wanted to make `Maybe a` an instance of `Eq`, we could write:

```haskell
instance Eq a => Eq (Maybe a) where
  (==) (Just x) (Just y) = x == y
  (==) Nothing Nothing   = True
  (==) _ _               = False
```
Note that we have a typeclass constraint on `a` being in `Eq`, since otherwise we could not have `x == y` in the second line. 

## Automatic derivation

It turns out that GHC can automatically derive instances for certain typeclasses.  Instead of manually writing `instance Eq Color` above, we can just write 

```haskell
data Color = Red | Green | Blue
  deriving (Read, Show, Eq, Ord)
```

On the type `Color`, this will automatically implement functions for string parsing (`Read`), string printing (`Show`), equality testing (`Eq`), and ordering (`Ord`).

### References

[_Real World Haskell_, Chapter 6](http://book.realworldhaskell.org/read/using-typeclasses.html).
