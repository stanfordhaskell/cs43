{-# LANGUAGE GADTs #-}

module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Expr a where
    I    :: Int  -> Expr Int
    B    :: Bool -> Expr Bool
    O    :: Ordering -> Expr Ordering
    Add  :: Expr Int -> Expr Int -> Expr Int
    Mul  :: Expr Int -> Expr Int -> Expr Int
    Eq   :: Eq a => Expr a -> Expr a -> Expr Bool
    Comp :: Ord a => Expr a -> Expr a -> Expr Ordering
    If   :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2
eval (Comp e1 e2) = compare (eval e1) (eval e2)
eval (If p e1 e2) = if eval p then eval e1 else eval e2
