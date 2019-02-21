{-# LANGUAGE FlexibleContexts #-}

module Main where

import Calculator (interpret)
import Data.Map.Strict as M
import Control.Monad

--------------------
-- Example 1: Maybe
--------------------

safeInv :: Float -> Maybe Float
safeInv 0 = Nothing
safeInv x = Just $ 1/x

val = Just 5 >>= safeInv

val' = do
  x <- Nothing
  y <- Just 6
  safeInv (x + y)

val'' =
  Just 5 >>= (\x ->
  Just 6 >>= (\y ->
  safeInv (x + y)))

inventory = M.fromList [("0001", 5.99), ("0002", 12.99)]

comparePrice i1 i2 = do
  p1 <- M.lookup i1 inventory
  p2 <- M.lookup i2 inventory
  pure $ compare p1 p2

------------------------
-- Example 2 ----------------------

move (x,y) = [(x, y), (x + 1, y), (x, y + 2)]

move3steps = do
  x <- pure (0,0)
  y <- move x
  z <- move y
  move z

prod xs ys = [(x,y) | x <- xs, y <- ys, x == y]

prod' xs ys = do
  x <- xs
  y <- ys
  guard (x == y)
  pure (x,y)


-------------------










main = do
  x <- getLine 
  let output = x ++ "!"
  putStrLn output




