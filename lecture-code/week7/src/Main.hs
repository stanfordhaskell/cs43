{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.Loops hiding (iterateUntil)

main = putStrLn "hi"

-- TODO mention before class
-- . ask about last assignment completion, solutions
-- . revisit requirements, two more assignments
-- . mention next lectures, poll interest

-------------------------------
-- Example 4a: Reader
-------------------------------

mean :: (Fractional a) => [a] -> a
mean = do
    s <- sum
    l <- length
    pure $ s / fromIntegral l

------------------------------
-- Example 4b: Reader
------------------------------

sum' :: (Fractional a) => Reader [a] a
sum' = reader sum  -- think Reader sum

-- ghci> runReader sum' [1,2,3]

length' :: Reader [a] Int
length' = reader length

mean' :: (Fractional a) => Reader [a] a
mean' = do
    s <- sum'
    l <- length'
    pure $ s / fromIntegral l

-- ghci> runReader mean' [1,2,3]



data Config = Config { width :: Float
                     , depth :: Float
                     , height :: Float }

testConfig = Config { width = 5, depth = 6, height = 10 }

area :: Reader Config Float
area = do
    Config w d h <- ask
    pure (w * d)

-- ghci> runReader area testConfig

area' :: Reader Config Float
area' = do
    w <- asks width -- asks = reader (TODO think about types in class)
    d <- asks depth
    pure (w * d)

-- ghci> runReader area' testConfig

volume :: Reader Config Float
volume = do
    a <- area
    h <- asks height
    pure (a * h)

-- ghci> runReader volume testConfig



----------------------------------
-- Example 5: Writer
----------------------------------

-- motivation, logging

collatz :: Int -> Int
collatz n = if odd n then 3 * n + 1 else n `div` 2

-- ghci> collatz 5

-- ghci> until (== 1) collatz 25

collatz' :: Int -> (Int, String)
collatz' n = if odd n
                then (3 * n + 1, "odd, ")
                else (n `div` 2, "even, ")

applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- ghci> (25, "") `applyLog` collatz'

collatzParity :: Int -> Writer String Int
collatzParity n = if odd n
                     then writer (3 * n + 1, "odd, ")
                     else writer (n `div` 2, "even, ")

-- ghci> runWriter (writer (25, "") >>= collatzParity)
-- ghci> runWriter (writer (25, "") >>= collatzParity >>= collatzParity)

-- TODO look at type of until,
-- then hoogle (a -> Bool) -> (a -> m a) -> a -> m a

-- ghci> runWriter $ iterateUntilM (== 1) collatzParity 25

collatzValues :: Int -> Writer [Int] Int
collatzValues n = do
    let value = if odd n then 3 * n + 1 else n `div` 2
    writer (value, [value])
    

-- ghci> runWriter $ iterateUntilM (== 1) collatzValues 25


-----------------------------------
-- Example 6: State
-----------------------------------


