module Main where

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

--- Generalizing some basic functions ---

-- Doubling
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

-- Multiplying
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList m [] = []
multiplyList m (x:xs) = m * x : multiplyList m xs

-- Recovering doubleList
doubleList' xs = multiplyList 2 xs
doubleList''  = multiplyList 2

-- Applying arbitrary functions
applyToIntegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applyToIntegers _ [] = []
applyToIntegers f (x:xs) = (f x) : applyToIntegers f xs

-- Recovering multiplyList
multiplyList' m xs = applyToIntegers (* m) xs
multiplyList'' m = applyToIntegers (* m)

map' :: (a -> b) -> [a] -> [b] 
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs


filter 


