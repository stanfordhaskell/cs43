module Main where

import Data.Semigroup (stimes) -- stimes not imported by default


main :: IO ()
main = do
  putStrLn "hello world"


newtype Product = Product Int deriving (Show)
newtype Sum = Sum Int deriving (Show)

instance Semigroup Sum where
    _ <> _ = undefined -- define <>
    
    stimes _ _ = undefined -- define O(1) stimes
    -- note the type is of stimes :: (Integral b) => b -> a -> a
    --   To use the first argument as a value of type `Int`, you can use the
    --   fromIntegral function to cast any `Integral` value to any other `Num`
    --   value, for example if `x` has a type in typeclass `Integral` then
    --   `fromIntegral x` can be any type (for example, `Int`) in the typeclass
    --   `Num`. If you get stuck, write the Product instance first.
    
instance Semigroup Product where
    _ <> _ = undefined -- define <>
    
    stimes _ _ = undefined -- define O(1) stimes


stimes' :: (Semigroup a, Integral b) => b -> a -> a
stimes' n x
  | n <= 0    = error "positive multiplier expected"
  | n == 1    = x
  | otherwise = undefined -- implementation here
  -- you can add guards or local functions with "where"
  -- hint, use the `even` function
