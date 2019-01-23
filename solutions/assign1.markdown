---
title: Assignment 1
---

## Problems

Pick three of the following.

1. Implement the `map` function using a fold. 

    ```haskell
    map' f = foldr (\ x xs -> f x : xs) []
    ```

2. Implement the `filter` function using a fold. 

    ```haskell
    filter' p xs = foldr step [] xs
        where step x ys | p x          = x : ys
                        | otherwise    = ys
    ```

3. Implement `foldl` using `foldr`.

    ```haskell
    foldl' :: (a -> b -> a) -> a -> [b] -> a

    foldl' f z xs = foldr step id xs z
        where step x g a = g (f a x)
    ```

4. Write code to compute the smallest positive number that is evenly divisible
   by all the numbers from 1 to $n$.  Provide an answer for $n = 20$.

    ```haskell
    myGCD :: Integral a => a -> a -> a
    myGCD x 0 = x
    myGCD x y = myGCD y (x `mod` y)

    myLCM :: Integral a => a -> a -> a
    myLCM x y = (x * y) `div` (myGCD x y)

    main = putStrLn . show $ n
         where
            n = foldl myLCM 1 [1..20]
    ```

    `Output: 232792560.`

5. Write code to compute the $n$th prime number.  Provide an answer for $n = 10001$.

    ```haskell
    -- See https://wiki.haskell.org/Prime_numbers
    -- for several optimized implementations.

    primesTo m = sieve [2..m]
      where
        sieve (p:xs) 
            | p*p > m = p : xs
            | True    = p : sieve [x | x <- xs, rem x p > 0]

    main = putStrLn . show $ (ps !! 10000)
      where
        ps = primesTo 1000000
    ```

    `Output: 104743.`


## References

- Problem 3 is from Real World Haskell.
- Problems 4 and 5 are from Project Euler.
