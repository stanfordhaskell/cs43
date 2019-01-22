module Main where

import Data.Semigroup (stimes) -- stimes not imported by default


main :: IO ()
main = do
  putStrLn "hello world"


newtype Sum = Sum Int
newtype Product = Product Int

instance Semigroup Sum where
    _ <> _ = undefined -- define <>
    
    stimes _ _ = undefined -- define O(1) stimes
    
instance Semigroup Product where
    _ <> _ = undefined -- define <>
    
    stimes _ _ = undefined -- define O(1) stimes


stimes' :: (Semigroup a, Integral b) => b -> a -> a
stimes' n x
  | n <= 0    = error "positive multiplier expected"
  | n == 1    = x
  | otherwise = undefined -- implementation here
  -- you can add guards or local functions with "where"
