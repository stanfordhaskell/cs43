---
title: Parallel Programming in Haskell
---

Last lecture, we saw how to write concurrent code in Haskell.  In these lecture notes, we will explore parallel programming in Haskell.

First, a review of terminology:

- _Parallelism_ refers to running a Haskell program on multiple processors (or machines), with the goal of improving performance.  We strive to do this "invisibly," minimizing the number of semantic changes we need to make to our code.

- _Concurrency_ refers to implementing a program on mulitple I/O-performing threads.  The primary goal of concurrency is not typically to gain performance, but because it is the most natural way to write the program of interest.

## Lazy Evaluation and Seq

We will start with a review of lazy evaluation. Haskell is a lazy programming language, which means that expressions are not evaluated until they are equired.

Suppose we type the following in GHCI.

```haskell
> let x = 1 + 2 :: Int
```

Note that at this point, `x` is unevaluated -- the `1+2` has not been converted to `3` yet.  To see this, we can use the `:sprint` command in GHCI.  Suppose we do this:

```
> :sprint x
x = _
```

Here, the `_` symbol means that `x` is unevaluated.  Now, if we type `x` at the prompt, we require that it is evaluated.  Then we get:

```
> x
3

> :sprint x
x = 3,
```

so the thunk representing `1+2` is overwritten by the integer 3.

Suppose we have the following scenario:

```
> let x = 1 + 2 :: Int
> let y = x + 1
```

If we run `:sprint` on `x` and `y`, we get `_`.  It turns out that we can use the built-in `seq` function to force evaluation.  So if we run

```haskell
> seq y ()
()
```

The `seq` functions evaluates its first argument, here `y`, and returns its second argument.  Now, we can see that both `x` and `y` have been evaluated:

```haskell
> :sprint x
x = 3
> :sprint y
y = 4
```

## Parallel sorting

Let's recall one standard way of writing a sorting function:

```haskell
sort :: (Ord a) => [a] -> [a]
sort (x:xs) = lesser ++ x:greater
    where lesser  = sort [y | y <- xs, y <  x]
          greater = sort [y | y <- xs, y >= x]
sort _ = []
```

Now, it turns out we can parallelize this code fairly easily.  Here's an implementation:

```haskell
import Control.Parallel (par, pseq)

parSort :: (Ord a) => [a] -> [a]
parSort (x:xs)    = force greater `par` (force lesser `pseq`
                                         (lesser ++ x:greater))
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _         = []
```

We have made very few modifications to the code itself.  We have added three functions: `par`, `pseq`, and `force`.

The `par` function "indicates that it may be beneficial to evaluate the first argument in parallel with the second" (source: Control.Parallel docs).  It does something very similar to `seq`, evaluating the left argument to [weak head normal form](https://wiki.haskell.org/Weak_head_normal_form) (WHNF), and returning the second.

`pseq` is also quite simliar to `seq`.  It evaluates the expression to WHNF before returning the expression on the right.  In contrast to `seq`, `pseq` is only strict in its first argument, and guarantees that the left argument is evaluated before the right.

And lastly, `force` is a function that forces the entire spine of the list to be evaluated before we get back a constructor.  It looks like this:

```haskell
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1
```

Here, we just walk down its spine to the end, then use `pseq` once.  To see why this is necessary, we note that the following function would fail to do any meaningful parallel work:

```haskell
sillySort (x:xs) = greater `par` (lesser `pseq`
                                  (lesser ++ x:greater))
    where lesser   = sillySort [y | y <- xs, y <  x]
          greater  = sillySort [y | y <- xs, y >= x]
sillySort _        = []
```

To see why, we recall that evaluation to WHNF only computes enough of an expression to see its outermost constructor.  In other words, we would only be forcing the evaluation of the first element of each sorted sublist.  Thus, we need the additional `force` to ensure that the body of the list gets evaluated.

We note that it actually will take some tuning to make `parSort` faster than the sequential sorting algorithm (since sorting is a task that is fairly hard to parallelize).  But we emphasize that we were able to parallelize sorting with hardly any semantic changes to our sorting algorithm.

## Parallel Strategies

Parallel evaluation strategies, found in the `Control.Parallel.Strategies` module, allow you to separate the description of the parallelism from the logic of your program.  While earlier, we had to intersperse calls to `par` and `pseq` in application logic, `Strategies` allow us to separate this.

The library introduces a strategy type s follows:
```haskell
type Strategy a = a -> Eval a
```

Here, `Eval` is a monad that makes it easier to define parallel strategies.  It is a strict identity monad, so that in bind call
```haskell
m >>= f,
```
`m` is evaluated before the result is called to `f`.

An evaluation strategy does no computation; it simply ensures that a value is evaluated to some extent.  The simplest strategy is named `r0`, and does nothing:

```haskell
r0 :: Strategy a
r0 x = return x
```

Next is `rseq`, which evaluates a value to weak head normal form, using [`evaluate`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:evaluate).
```haskell
rseq :: Strategy a
rseq x = Eval (evaluate x)
```

And there are many other strategies that can be expressed compositionally, which can be found in the docs [here](http://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html#t:Eval).

## Parallel map

Normally, we might write a parallel implementation of `map` as follows, interspersing calls to `par` with function logic.  We might have something like the following:

```haskell
import Control.Parallel (par)

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) = let r = f x
                       in r `par` r : parallelMap f xs
parallelMap _ _      = []
```

And alternate way to do this with `Strategies` is to use the `parList` function, which applies an evaluation strategy in parallel to every element of a list.  This isn't how it is implemented in the library, but semantically, this function does something like this:

```haskell
paraList :: Strategy a -> Strategy [a]
parList strat [] = ()
parList strat (x:xs) = strat x `par` (parList strat xs)
```

Then, parallel map can be written like this:

```haskell
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat
```
where `using :: a -> Strategy a -> a` evaluates a value using the given strategy.

For instance, if we want to use the `rpar` strategy, which sparks its argument for evaluation in parallel, we could do something like this:

```haskell
doubles = parMap rpar (\x -> x*2) [1..100]
```

## Bonus example: K-Means Clustering

An interesting example of how to apply these concepts in practice can be seen in the K-Means algorithm.  The algorithm takes the number of clusters to find as a parameter, and makes an initial guess at the center of each cluster.

- Assign each point to the cluster to which it is closest.  This yields a new set of clusters.

- Find the centroid of each cluster.

- Repeat steps 1 and 2 until the cluster locations stabilize.  (For our purpose, we will stop the algorithm, after a arbitrary number of iterations).

We represent a data point as follows:
```haskell
data Point = Point !Double !Double,
```
and define various operations on points:
```haskell
zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2) + ((y1-y2)^2)
```

We define clusters with the following type:
```haskell
data Cluster
  = Cluster { clId :: Int
            , clCent :: Point
            }
```
which contains its number (`clId`) and its centroid (`clCent`).

We also define an intermediate type called `PointSum`, which represents the sum of a set of points.  It contains the number of points in the set and the sum of their coordinates.

We need a few more helper functions, to repeatedly add points  to a `PointSum`:

```haskell
addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y)
  = PointSum (count+1) (xs + x) (ys + y)
```

A `PointSum` can be turned into a `Cluster` by computing the centroid.
```haskell
pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) =
  Cluster {clId = i
          , clCent = Point (xs / fromIntegral count) (ys / fromIntegral count)
          } 
```

Now, a sequential implementation of `K-means` looks like this:

```haskell
-- <<step
step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points
   = makeNewClusters (assign nclusters clusters points)
-- >>

-- <<assign
assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
    vec <- MVector.replicate nclusters (PointSum 0 0 0)
    let
        addpoint p = do
          let c = nearest p; cid = clId c
          ps <- MVector.read vec cid
          MVector.write vec cid $! addToPointSum ps p

    mapM_ addpoint points
    return vec
 where
  nearest p = fst $ minimumBy (compare `on` snd)
                        [ (c, sqDistance (clCent c) p) | c <- clusters ]
-- >>

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster i ps
  | (i,ps@(PointSum count _ _)) <- zip [0..] (Vector.toList vec)
  , count > 0
  ]

kmeans_seq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters =
  let
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do                  -- <1>
        putStrLn "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = step nclusters clusters points    -- <2>
        if clusters' == clusters                          -- <3>
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

tooMany = 80
```

To parallelize it, we can break up the `assign` function, since it is essentially just a `map` over the points.  This would look like this:

```haskell
-- <<kmeans_strat
kmeans_strat :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_strat numChunks nclusters points clusters =
  let
      chunks = split numChunks points                            -- <1>

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        printf "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = parSteps_strat nclusters clusters chunks -- <2>
        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters
-- >>

-- <<split
split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs
-- >>
```

For the full code example, see https://github.com/simonmar/parconc-examples/tree/master/kmeans, and chapter 3 of Simon Marlow's "Parallel and Concurrent Programming in Haskell."

## References

- Some of this material was adapted from Real World Haskell, [Chapter 24](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html)

- The KMeans example was adapted from https://github.com/simonmar/parconc-examples

- Docs of [`Control.Parallel.Strategies`](http://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html).

