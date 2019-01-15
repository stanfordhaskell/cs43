module Main where

-- Source: Haskell wikibook
main :: IO ()
main = do
  putStrLn "Lecture 3 notes"

---------------------------------
---------------------------------
-- Higher Order Functions
---------------------------------
---------------------------------

--- List-based recursion ---
length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

-- length' [1,2,3,4];
--  x = 1
--  xs = [2,3,4]

--- Generalizing some basic functions ---

-- Doubling a list
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

-- : on left: deconstruction
-- : on right: construction

-- take 10 $ doubleList [1..]

-- Multiplying
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (x:xs) = m * x : multiplyList m xs

-- Recovering doubleList
doubleList' xs = multiplyList 2 xs
doubleList''  = multiplyList 2

-- Applying arbitrary functions
applyToIntegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToIntegers _ [] = []
applyToIntegers f (x:xs) = (f x) : applyToIntegers f xs

-- Recovering multiplyList
multiplyList' m xs = applyToIntegers ((*) m) xs
multiplyList'' m = applyToIntegers ((*) m)



-- Map'
map' :: (a -> b) -> [a] -> [b] 
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

-- map ((+) 3) [1..100]
-- map (map (^2)) [[1,2,3,4],[5,6,7,8]]  


-- Filter'

filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = [ x | x <- xs, p x ]

-- filter (`elem` ['a'..'z']) "FuNcTiOnAL PrOgRaMmInG"
-- filter (> 10) [1..100]
-- filter (\x -> length x > 4) ["aaaaa", "bb", "CCCCC"]

---------------------------------
---------------------------------
-- Folds, scans, and laziness
---------------------------------
---------------------------------

-- Laziness example: evens
-- take 10 $ filter even [1..]
--
-- Be careful: better to use null instead of length for emptiness checks
-- length $ filter even [1..]

-- Laziness example: quicksort
-- Technically not optimal, because it's not inplace, and uses concatenation.  But still interesting.
quickSort [] = []
quickSort (x:xs) = (quickSort lesser) ++ [x] ++ (quickSort greater)
  where
    lesser = filter (< x) xs
    greater = filter (>= x) xs

-- head $ quicksort xs - will only be O(n)!
-- take k $ quicksort xs - only the first k elements will be sorted.
-- This will take O(n + k log k) time, whereas a non-lazy quicksort
-- would take O(n log n) time.
-- For more: see http://wiki.c2.com/?QuickSortInHaskell

-- Folds
-- A common pattern of recursion
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- stdlib provides four fold functions:
-- foldr, foldl, foldr1, foldl1
-- foldr (+) 0 [1..10]

-- right-associative fold:
-- First argument is a function with two arguments
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc []     = acc
foldr' f acc (x:xs) = f x (foldr f acc xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc []     = acc
foldl' f acc (x:xs) = foldl f (f acc x) xs

-- foldr f acc (a:b:c:[]) = f a (f b (f c acc))
-- foldl f acc (a:b:c:[]) = f (f (f acc a) b) c

-- foldr1: does not need an explicit zero for an accumumlator,
-- uses last element of the list instead.

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
foldr1' _ [] = error "empty list"

-- Right-associative tends to be more natural in Haskell.
-- Right folds can operate on infinite lists; compiler will know when to stop.

-- Important questions: why does foldr work on infinite lists but not foldl?
-- Foldr evaluates "outside-in", while foldl evaluates "inside-out"

echoes :: [Int] -> [Int]
echoes = foldr (\ x xs -> (replicate x x) ++ xs) []

-- take 10 (echoes [1..])

-- Scan is like a hybrid between a map and a fold.
-- take 10 $ scanl1 (+) [1..]

{-import Data.Complex-}

{-type C = Complex Double-}
{-type Pnt = (Double, Double)-}
{-type Grid a = [[a]]-}

{-mandelbrot :: C -> [C]-}
{-mandelbrot c = iterate (\z -> z * z + c) (0.0 :+ 0.0)-}
