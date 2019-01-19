---
title: Assignment 1
---

## Problems

Pick three of the following.

1. Implement the `map` function using a fold. 

```haskell
map' f = foldr (\ x xs -> f x : xs) []
```

1. Implement the `filter` function using a fold. 

```haskell
filter' p xs = foldr step [] xs
    where step x ys | p x          = x : ys
                    | otherwise    = ys
```

1. Implement `foldl` using `foldr`.

```haskell
foldl' :: (a -> b -> a) -> a -> [b] -> a

foldl' f z xs = foldr step id xs z
    where step x g a = g (f a x)
```

1. Write code to compute the smallest positive number that is evenly divisible
   by all the numbers from 1 to $n$.  Provide an answer for $n = 20$.

1. Write code to compute the $n$th prime number.  Provide an answer for $n = 10001$.
