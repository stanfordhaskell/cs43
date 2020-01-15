---
title: Expressions, Values, and Types
---

## Every expression has a value

Variables in Haskell are immutable; once defined, their values never change. For
this reason, the following code gives an error

```haskell
x = 1
x = 2 -- error "multiple declarations of x"
```

and the order of definitions does not matter so long as all variables are defined.

```haskell
z = x + 1
x = 1
```

Thus, since variables and more generally expressions in Haskell never change in 
value, any expression can be replaced with its value at any point in a Haskell
program to no effect.^[This property, known as "referential transparency," is
what makes Haskell a "pure" language.]

In this sense, Haskell variables are similar to variables in mathematics where, for
example, the $x$ in $2 x + 1 = 0$ has a single unchanging value and replacing it
with that values does not change the truth of the statement.

If variables cannot change, how does Haskell do anything useful? This first
answer to this question is provided by functions.

```haskell
inc x = x + 1

absMax x y = if abs x > abs y
                then abs x
                else abs y
```

We can use these functions in `ghci` or elsewhere in our code.

```haskell
ghci> inc 4
5
ghci> absMax (-4) 2
4
```

Functions in Haskell are values like any other. While they cannot be printed in
the REPL since Haskell does not know how to print function values,

```haskell
ghci> inc
```

```error
error:
    • No instance for (Show (Integer -> Integer))
```

we can pass functions as arguments to other functions.

```haskell
applyTwice f x = f (f x)
```

This lets us define higher-order functions that control the behaviour of other
functions and enable higher levels of abstraction.

```haskell
ghci> applyTwice inc 3
5
ghci> applyTwice not True
True
```

Since Haskell values are immutable and therefore cannot change during runtime,
programs give different results only by treating the outside world as an input
to a function which the program defines.^[Don't worry about this too much, it
will become much clearer later on.]

The most basic representation of a function in Haskell is as an unnamed,
anonymous function value.

```haskell
ghci> (\x -> x + 1) 5
6
```

Haskell has plenty of syntactic sugar for defining functions. For example, the
function definitions above (repeated here)

```haskell
inc x = x + 1
absMax x y = if abs x > abs y then abs x else abs y
applyTwice f x = f (f x)
```

are nothing but syntatic sugar for naming anonymous functions.

```haskell
inc = \x -> x + 1
absMax = \x y -> if abs x > abs y then abs x else abs y
applyTwice = \f x -> f (f x)
```

In Haskell functions are applied simply by writing them before their arguments.
The only exception is infix functions like `+` which are written between their
arguments, though surrounding them in parentheses makes them act like normal
functions.

```haskell
ghci> 1 + 2
3
ghci> (+) 1 2
3
```

While function application is implicit in Haskell, there is also a function
application operator `$`. 

```haskell
infixr 0 $ -- defines right associate infix operator of lowest precedence
($) f x = f x
```

While this might seem redundant, one use for `$` is enabled by it having the
lowest precedence of any operator.  This allows it to replace parentheses in
many expressions as follows.

```haskell
f (g (h x)) == f $ g $ h x
```

## Every value has a type

Every value in Haskell, and therefore every expression with that value, has a
type.

```haskell
ghci> :t True
True :: Bool
```

Some expressions can represent different values with different types depending
on context^[This is known as polymorphism.], for example written out integers can
represent any of Haskell's number types.

```haskell
ghci> :t 1
1 :: Num p => p
```

This type signature says that `1` can be of any type `p` so long as `p` is part
of the number type class `Num`.^[This will be explained in great detail when we cover
typeclasses, but for now it is sufficient to know that `Num` represents a set of
types for which the usual numerical operations are defined.] Here, `p` is a type
variable. Everything before `=>` in a type signature is a constraint
on its type variables. When we want a polymorphic expression to have a specific
type we can annotate it.

```haskell
ghci> :t (1 :: Int)
(1 :: Int) :: Int
ghci> :t (1 :: Float)
(1 :: Float) :: Float
```

Since functions are values, they also have types.

```haskell
ghci> :t not
not :: Bool -> Bool
ghci> :t (+)
(+) :: Num a => a -> a -> a
```

Here `(+)` takes two values of some type `a` and returns a value of type `a`, so
long as `a` is a type in the `Num` typeclass. Haskell is strongly typed, meaning
that functions cannot be applied to values of the wrong type and such errors are
caught at compilation.

```haskell
ghci> not (1 :: Int)
```
```error
error:
     • Couldn't match expected type ‘Bool’
       with actual type ‘Int’
```

One of Haskell's strongest features is type inference. Every expression in
Haskell has an associated type, but one rarely needs to write this out
explicitely since Haskell can usually figure out the type on its own. For
example, when we defined `inc` Haskell inferred the type for us.

```haskell
ghci> :t inc
inc :: Num a => a -> a
```

Just like written integers, the `inc` function is polymorphic, and returns a
value of its input type so long as that input type is in `Num`.

```haskell
ghci> :t inc (1 :: Int)
inc (1 :: Int) :: Int
```

While Haskell's ability to infer types is powerful, it is often a good idea to
specify the type of a function by writing the type above its definition. This
can be very useful in reasoning about code, and also forces the function to have
the prescribed type even if Haskell could give it a more general one. Thus, we
could write^[Note that Haskell's infix functions can be written as normal
functions that take their arguments on the right by surrounding them in
parentheses.]

```haskell
incInt :: Int -> Int
incInt = inc
```

and this annotated function could not be passed a value of any type besides
`Int`.

```haskell
ghci> incInt (1 :: Float)
```
```error
error:
    • Couldn't match expected type ‘Int’
      with actual type ‘Float’
```

## Currying

Haskell functions are applied to only one argument, and function application is
evaluated left to right, i.e. is left associative. This means that a function
`f` of two arguments `f x y` is correctly interpreted as `f` being applied to
`x` to give a function which is applied to `y`, i.e.

```haskell
f x y == (f x) y
```

The type signature of `f` hints at this as well.

```haskell
f :: a -> b -> c
```

This says that `f` takes a value of type `a` and returns a value of type `b ->
c`, which is a function that takes a value of type `b` and returns a value of
type `c`. Here, the `->` symbol in the type signature is applied right to left,
i.e. it is right associative. Thus, the type above can be written as 

```haskell
f :: a -> (b -> c)
```

Note that function application associates to the left and `->` associates to the
right because the first or innermost function application corresponds to the
outermost `->`. If you don't see this right away, it is a good point to dwell
on.

This property allows us to simplify many definitions using equational reasoning,
i.e. rewriting definitions as one might in math.^[Equational reasoning will be a
reccuring theme. Here, we are translating a function into so-called pointfree
style. More will be said about this later on.]

```haskell
inc x = ((+) 1) x
inc   = (+) 1
```

Here `(+) 1` is a partial application of `(+)`. 

```haskell
(+) ::  Num a => a -> (a -> a)
(+) 1 ::  Num a => a -> a 
(+) 1 2 :: Num a => a
```
