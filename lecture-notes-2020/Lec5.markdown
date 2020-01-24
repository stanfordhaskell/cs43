We will start by discussing some basic ideas pertaining to types.  This class isn't

Here are some examples:

The boolean type can be defined as follows:

`data Bool = True | False`

The unit type is defined as follows:

`data () = ()`

The Ord type has three possible values:

`data Ord = LT | EQ | GT`

We can even define a type that corresponds to natural numbers:

`data Natural = Z | S Natural`

In all of these cases, we can talk about a notion of "cardinality" associated with each type.  Namely:

```
|Bool| = 2
|Ord| = 3
|Int| = 2^64
|Integer| = inf
```

Suppose we have a datatype `BoolAndOrd` defined as follows:
```
data BoolAndOrd = BoolAndOrd Bool Ord.
```
Here,
```
|BoolAndOrd| = 6
```

Similarly, we can define a datatype `BoolOrOrd` as follows:
```
data BoolOrOrd = ABool Bool | AnOrd Ord
```

We can also define a type of functions from Ord -> Bool,
```
data OrdToBool = OrdToBool (Ord -> Bool),
```
and
```
|OrdToBool| = 2^3.
```

Note that all these values assume that the function values exist / don't crash, etc.

We can define the "sum," "product", and "function" types as follows:

```
data Sum a b = First a | Second b

data Prod a b = Prod a b

data Func a b = Func (a -> b)
```

In cardinalities, we have
```
|Sum a b| = |a| + |b|
|Prod a b| = |a| * |b|
|Func a b| = |b| ^ |a|
```

Let's create a new type
```
data OurType = Con1 Bool Bool Ord | Con2 Bool Ord,
```
which has (2 * 2 * 3) + (2 * 3) = 18 possible values.

We can use types to show that (a + b) * (c + d) = `a*c + a*d + b*c + b*d`.

We can start with

```
data LHS a b c d = LHS (Either a b) (Either c d)
data RHS a b c d = RHS1 a c | RHS2 a d | RHS3 b c | RHS4 b d
```

In a sense, these types are the same.  We can show this by explicitly writing out the isomorphism between the two types.

We have:
```
isoLR :: LHS a b c d -> RHS a b c d
isoLR (LHS (Left a) (Left c)) = RHS1 a c
isoLR (LHS (Left a) (Right d)) = RHS2 a d
isoLR (LHS (Right b) (Left c)) = RHS3 b c
isoLR (LHS (Right b) (Right d)) = RHS4 b d
```

Similarly, we could define an `isoRL` function so that `isoLR . isoRL = id`.

## More on functors

We can define a type 
```
data Identity a = Identity a
  deriving (Show)
```

And we can write the functor instance as follows:
```
instance Functor Identity where
  fmap :: (a -> b) -> f a -> f b
  fmap f (Identity x) = Identity (f x)
```

Another type:
```
data Pair a = Pair a a
  deriving (Show)
```

```
instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x y) = Pair (f x) (f y)
```

And recall that the functor instance for Maybe looks like this:

```
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
```

How do we make a functor instance for Sum?  Since it has kind * -> * -> *, we need to "partially apply" it: we'd have

```
instance Functor (Sum a) where
  fmap :: (b -> c) -> Sum a b -> Sum a c
  fmap f (First x) = First $ x
  fmap f (Second y) = Second $ f y
```

```
instance Functor (Prod a) where
  fmap :: (b -> c) -> Prod a b -> Prod a c
  fmap f (Prod x y) = Prod x (f y)
```

And finally, we can write a functor instance for functions:
```
instance Functor (Func a) where
  fmap :: (b -> c) -> Func a b -> Func a c
  fmap f (Func g) = Func (f . g)
```

## An example

There is a type of functions from e -> a called Reader:

```
data Reader e a = Reader { runReader :: e -> a }
```

Intuition: there is some environment, and a function takes in the environment and returns a value.

Let's suppose we have a game, with types associated with it:

```
data Tile = Wall | Path | Forest

data Player = Player { name :: String
                     , health :: Float }

data GameState = GameState { player :: Player
                           , level :: Int }
```

We can define

```
readPlayer :: Reader GameState Player
readPlayer = Reader player
```

One way to write readName is:
```
readName :: GameState -> String
readName (GameState player level) = name player
```

But better is just:
```
readName :: GameState -> String
readName = name <$> readPlayer
```
