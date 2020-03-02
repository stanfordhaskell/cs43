---
title: Assignment 4
---

This two-part assignment introduces a new design pattern in one part and a new
monad for truly effectful computations in the other. It requires only a small
amount of new code to be written, but reading through the code carefully and
working out the types of various expressions in your head / in ghci will be
important both to complete the code without too much trial and error and
to get the most out of the assignment.

## Setup

Download the starter code from the [repo](https://github.com/ischeinfeld/cs43-assignments). 

## Problems

### Problem 1

The `assignment4-rio` folder is a stack project with the following directory structure.

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── assignment4-rio.cabal
├── src
│   ├── Main.hs
│   ├── Main1.hs
│   ├── Main2.hs
│   ├── Main3.hs
│   ├── Main4.hs
│   ├── Main5.hs
│   ├── Main6.hs
│   └── RIO.hs
├── stack.yaml
└── stack.yaml.lock
```

This problem should be solved by reading and editing the files `Main1.hs`
through `Main6.hs` in order. These implement a very simple program,
demonstrating a rewrite using the ReaderT design pattern. This is an approach to
writing programs centered around the monad instance for `ReaderT env IO a`. If
you want to know more about this design pattern,
[this](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) blog post goes
into great depth. 

### Problem 2 - The ST Monad

The`ST` monad in `Control.Monad.ST` allows us to write programs that use arbitrary _mutable state_, implemented as actual mutable memory on the machine.  As detailed in Launchbury and Jones' ["Lazy Functional State Threads"](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf), the `ST` monad maintains full referential transparency.  `ST` is commonly used to implement mutable arrays and other data structures. Intuitively, the `ST` monad and `STRefs`  (references to mutable cells) allow for controlled mutability in otherwise pure programs.  

# footnote: see https://stackoverflow.com/questions/5545517/difference-between-state-st-ioref-and-mvar, https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-ST.html for more details.

There are four key functions that are used when we operate on the `ST` monad.  Namely:
```
runST :: (forall s. ST s a) -> a
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
modifySTRef :: forall s a. STRef s a -> (a -> a) -> ST s ()
```

Here:
- `runST` takes stateful code and makes it pure again.
- `newSTRef` creates an `STRef`, a place in memory to store values.
- `readSTRef` reads the value from an `STRef`.
- `writeSTRef` writes a new value into an `STRef`.
- `modifySTRef` mutates the contents of an `STRef`.

1. We've provided a skeleton implementation of `sumST`.  Fill in the `undefined` to complete the implementation.

2. Implement `foldlST`, a version of `foldl` that uses the `ST` monad under the hood.  Here, the type of `foldlST` is
```
foldlST :: (a -> b -> a) -> a -> [b] -> a,
```
as in the usual `foldl`.

3. Consider the following operation on a positive integer `n`

- If the number is even, divide it by two.
- If the number is odd, triple it and add one.
- (Repeat).

The [Collatz conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) states that if we repeatedly apply this process, we will eventually reach the number 1, regardless of the start value of `n`.

Translate the following pseudocode to Haskell, using the ST monad to update mutable `STRef`s.

```
Integer collatz(Integer n) {
    
    assert(n > 0, "n must be positive");
    
    Integer x = n;
    Integer count = 0;
    
    while (x != 1) {
        
        count = count+1;
        
        if (x % 2 == 0) {
            x = x/2;
        } else {
            x = 3*x+1;
        }
        
    }
    
    return count;
    
}
```




## Submission instructions

Send an email to cs43-win1920-staff@lists.stanford.edu with a .zip file with your code.
