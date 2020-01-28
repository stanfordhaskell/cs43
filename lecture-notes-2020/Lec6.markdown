Suppose we wanted to do fmap' (+) (Just 1) (Just 2).

This evaluates to (Just (+ 1)) (Just 2), but this won't work.

We can write a function funcMaybe that takes care of this:

```
funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
funcMaybe (Just f) (Just x) = Just (f x)
funcMaybe _ _ = Nothing
```

Introducing Applicative:

```
class Functor f => Applicative f where
  pure :: a -> f a 
  (<*>) :: f (a -> b) -> f a -> f b
```

We can define our own type of list and write an applicative instance for it.

```
newtype MyList a = MyList [a]
  deriving (Show)
```

We can write a Functor instance:

```
instance Functor MyList where
  fmap f (MyList l) = MyList (fmap f l)
```

And we can also write an applicative instance:

```
instance Applicative MyList where
  pure val = MyList [val]
  MyList fs <*> MyList xs = MyList [f x | f <- xs, x <- xs]
```

And indeed, if we write

```
(MyList [(*2), (-) 10]) <*> [1,2,3,4,5]
...
```

We can now define a new type MyZipList that has a different applicative instance.  Specifically:

```
newtype MyZipList a = MyZipList [a]
```

```
instance Functor MyZipList where
  fmap f (MyZipList l) = MyZipList (fmap f l)

instance Applicative MyZipList where
  pure = undefined
  (MyZipList fs) <*> (MyZipList xs) = MyZipList (zipWith ($) fs xs)
```

(Example)

Let's look at the applicative laws:


- identity
pure id <*> v = v
- composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
- homomorphism
pure f <*> pure x = pure (f x)
- interchange
u <*> pure y = pure ($ y) <*> u

An important consequence of the laws is
```
fmap f x = pure f <*> x
```

In other words, recall 
fmap (+1) [1,2,3]
> [2,3,4]
This is equivalent to
[(+1)] <*> [1,2,3]
> [2,3,4].


