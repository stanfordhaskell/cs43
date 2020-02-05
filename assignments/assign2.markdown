---
title: Assignment 2
---

## Set Up

*Note starter code updated Feb 4 so that tests compile without modification.*

Download the starter code from the [repo](https://github.com/ischeinfeld/cs43-assignments). 
The `assignment2` folder is a stack project with the following directory structure.

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── assignment2.cabal
├── src
│   ├── Exception1.hs
│   ├── Exception2.hs
│   ├── Problem1.hs
│   ├── Problem2.hs
│   ├── WeatherApp1.hs
│   └── WeatherApp2.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    ├── p1
    │   ├── Problem1Spec.hs
    │   └── Spec.hs
    ├── p2
    │   ├── Problem2Spec.hs
    │   └── Spec.hs
    └── p3
        ├── Exception2Spec.hs
        ├── Spec.hs
        └── WeatherApp2Spec.hs
```

All three problems below should be solved by editing the corresponding files in
`/src/`. It is configured (in the file `assignment1.cabal`) so that

```
$ stack test
```

runs all the tests and each problem's tests can be run individually, i.e. Problem 1
can be tested as follows.

```
$ stack test :p1
```

Passing all the tests and answering the written questions (as comments in code)
should be sufficient to answer all the problems.

To test out the code from a problem in ghci, run

```
$ stack ghci --no-load
```

where the `--no-load` flag prevents loading modules (since Problem3 has multiple overlapping
implementations in different modules). Then, you can load specific modules as follows.

```
> :l Problem1
```

## Problems

### Problem 1 - More about Functor

Recall, Functor is a typeclass which represents the ability to map over a type.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
```

We talked a lot about `fmap` but barely any about `(<$)`. Consider its type
signature and try it out on some values to see how it behaves. Then, write
`(<$)` (called `cmap` in the starter code `/src/Problem1.hs`) using `fmap`.
You may want to use the `const` function.

Because `(<$)` can be defined from `fmap`, specifying the
`fmap` function is all that is necessary to define a Functor instance.
However, `fmap` must follow certain rules. For the behavior of Functor to
be predictable, `fmap` must satisfy the identity law `fmap id == id`
and the composition law `fmap (f . g) == (fmap f) . (fmap g)`.

Below we define three datatypes with instances of Functor. For each,
either state that the functor laws are satisfied, argue that the type of `fmap`
as given is incorrect, or present an expression `a == b` that shows this
instance violates one of the functor laws. You can comment your answers in the
starter code file for this problem.

```haskell
data TwoVals = V1 | V2
data OneOf a b = First a | Second b 
data OneOrTwo a = One a | Two a a

instance Functor TwoVals where
  fmap f V1 = V2
  fmap f V2 = V1

instance Functor OneOf a where
  fmap f (First x) = First (f x)
  fmap f (Second y) = Second (f y)

instance Functor OneOrTwo where
  fmap f (One x) = One (f x)
  fmap f (Two x y) = Two (f y) (f y)
```

You'll notice that all of the illegal but well-typed examples above violated the
identity law flat-out -- there weren't any examples that preserved identity but
failed on composition. In fact, it is impossible to write a functor instance
that follows the identity law but violates the composition law. The reason we
state both laws (even though good identity implies good composition) is because
this link is somewhat subtle and the composition law states more explicitly what
we want from Functor. It also turns out that, if type has a functor instance, it
is unique! You can read [this](https://wiki.haskell.org/Typeclassopedia#Laws) section of
the Typeclassopedia for more on the Functor laws.

Consider the following weird-looking datatype.

```haskell
data LotsOfPieces a =
    Gone
  | Two a a 
  | RecTwo a a (LotsOfPieces a)
  | TreeLike a (LotsOfPieces a) (LotsOfPieces a)
  | TripleSandwich (LotsOfPieces a) (LotsOfPieces a) (LotsOfPieces a)
  deriving (Show)
```

Write the functor instance for `LotsOfPieces`. It shouldn't require a lot
of thinking -- in fact, because of all the weird parts, it should become more
clear that you're following a certain pattern in writing `fmap`. Briefly
describe the "algorithm" you were working through as you wrote the Functor
instance.

It turns out that due to this predictability Haskell can derive Functor
instances automatically. Comment out your Functor instance and add `Functor` to 
the list of deriving typeclasses. Check that the new automatically defined
`fmap` works the same on some simple functions as your implementation. You can
read about the `DeriveFunctor` language extension
[here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DeriveFunctor) if you are interested.
There are more subtleties involved than this problem would suggest, most of
which arrise when function types are part of your Functor instance. 

### Problem 2 - Streams

While the default list in Haskell allows infinite lists, we can go a step
further and define a type `Stream` that must be infinite.  Specifically,
streams are like lists, but with only a "cons" constructor -- there is no such
thing as an "empty" stream. Complete the datatypes and functions in
`/src/Problem2`.

### Problem 3 - Extending an exception library

In this problem, you will extend a library for dealing with exceptions to track
all exceptions that occur in a computation instead of just the first. To do so,
we will first learn about a new typeclass that will be used in our implementation.

So far, we have seen how typeclasses allow us to implement functions (for
example, `fmap` in the `Functor` typeclass) separately
for various types.^[This is known as *ad hoc* polymorphism, in contrast to the
*parametric* polymorphism provided by type variables.] A common
Haskell idiom is to create a typeclass for functions with a specific
structure and then let a value's type determine which function with that
structure to apply.
   
For example, consider the `Semigroup` typeclass (already implemented in Haskell
base, available in the Data.Semigroup module).

```haskell
class Semigroup a where
  (<>) :: a -> a -> a

  -- two more functions implemented in terms of <>
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

We will see many more examples of the power of this abstraction. For this part
of the assignment, you will extend a library representing exceptions and a
mock application using this library, using the `Semigroup` typeclass along the
way.

You are given `/src/Exception1.hs` and `/app/WeatherApp1.hs` and this problem
asks you to fill in `/src/Exception2.hs` and `/app/WeatherApp2.hs`. Read the
commented files in this order, filling in the latter two as marked (by `TODO`).
You can test your code by running

```
$ stack test :p3
```

and you can run code from the provided implementations as follows

```
$ stack ghci --no-load
> :l Exception1 WeatherApp1
> buildWeatherPage badUserAccess goodWeatherAccess -- for example
```

and from your extended implementations as follows.

```
> :l Exception2 WeatherApp2
```

You can switch back and forth between implementations in `ghci` by loading the
appropriate code, and you can reload the current implementation using `:r`.

## Submission instructions

Send an email to cs43-win1920-staff@lists.stanford.edu with a .zip file with your code.
