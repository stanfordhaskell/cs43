---
title: Typeclasses
---

## Motivation

We have seen that every Haskell function has a type signature that specifies
the types of its arguments. These types can be completely specific, as in

```haskell
doubleInt :: Int -> Int
doubleInt x = 2 * x
```

or can contain type variables, as in

```haskell
fst :: (a,b) -> a
fst (x,_) =  x
```

where `a` and `b` can be any type. While polymorphic functions like `fst` give 
us some flexibility in what types a function can accept, there are cases where
we might want the same function to work on different types in a manner that 
cannot be captured in a single polymorphic implementation. For example, we might
want a function

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
types. It can be seen as an extensible interface to some set of functions, and solves
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
function with type `a -> a -> Bool`. Many primitive Haskell types are already
instances of `Eq`, for example

```haskell
Prelude> 1 == 1
True
Prelude> True == False
False
```

However, assume we wanted to implement `(==)` for the type

```haskell
data Maybe a = Just a | Nothing
```

To do so, we need to make `Maybe a` an instance of the `Eq` typeclass. 

```haskell
instance Eq a => Eq (Maybe a) where
  (==) (Just x) (Just y) = x == y
  (==) Nothing Nothing   = True
  (==) _ _               = False
```
All we are doing here is defining `(==)` for the type `Maybe a`. Note that we have a
typeclass constraint on `a` being in `Eq`, since otherwise we could not have `x
== y` in the second line. 

## Some Common Typeclasses

### Eq

```haskell
class  Eq a  where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
```

### Ord

```haskell
class  (Eq a) => Ord a  where
  (<), (<=), (>=), (>)  :: a -> a -> Bool
  max, min              :: a -> a -> a
```

### Enum

```haskell

```

### Num

```haskell

```

### Show

### Read
