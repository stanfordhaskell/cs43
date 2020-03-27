# Review of Functor

We now introduce the Applicative typeclass.  Recall that we saw the Functor type class before, which provides a function fmap, with type.
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

For example, in GHCI we can type

```haskell
fmap (+1) (Just 5)
> Just 6
```

This allows us to apply a function of type `a -> b` to a `Maybe a`, resulting in a `Maybe b`.  But what if we have a function inside the Maybe context?  Suppose we want to take a `Maybe (a -> b)`, a `Maybe a`, and obtain a `Maybe b`.  We can write a function to do just this:

```haskell
funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
funcMaybe (Just f) (Just x) = Just (f x)
funcMaybe _ _ = Nothing
```

Note that the second pattern will match if either the function or the argument is `Nothing`, and intuitively we should return Nothing in this case.  This allows us to write
```haskell
funcMaybe (Just (+1)) (Just 2)
> Just 3
```

It turns out this is a common enough pattern that there is a typeclass for it called `Applicative`.  We'll show the definition below alongside the `Functor` typeclass for review:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Here, `pure` should be viewed as a minimal function to put a value of type `a` into the `Applicative` container.  `Maybe` is a member of the `Applicative` typeclass, and in this case `pure = Just`.  In fact, we can write an `Applicative` instance for `Maybe` as follows:

```haskell
instance Applicative Maybe where
  pure = Just
  (<*>) = funcMaybe
```

Now, we can write

```haskell
> Just (+1) <*> Just 2
> Just 3
```

which replaces the call to `funcMaybe` above.



