---
title: Assignment 1
---

## Set Up

Set up your local development environment, by following the instructions [here](/notes/Getting_Set_Up.html).

Create a Gitlab account, and familiarize yourself with the course repository.

## Problems

Pick three of the following.

1. Implement the `map` function using a fold. Your solution should look like the
   following for some `g` and `acc`.

   ```haskell
   map' f xs = foldr g acc xs
   map' f = foldr g acc   -- equivalent
   ```

1. Implement the `filter` function using a fold. Your solution should look like the
   following for some `g` and `acc`.


   ```haskell
   filter' f xs = foldr g acc xs
   filter' f = foldr g acc   -- equivalent
   ```
1. Implement `foldl` using `foldr`.

1. Write code to compute the smallest positive number that is evenly divisible
   by all the numbers from 1 to $n$.  Provide an answer for $n = 20$.

1. Write code to compute the $n$th prime number.  Provide an answer for $n = 10001$.

## Submission instructions

Send an email to cs43-win1819-staff@lists.stanford.edu with either:

- (Preferred) A link to a Gitlab / Github repository with your code.

- A .zip file with your code.

Include

1. Your Gitlab username.
1. Your code.
1. Answers for 4 or 5 if you choose them.
