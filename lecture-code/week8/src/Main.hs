{-# LANGUAGE GADTs #-}

module Main where

main :: IO ()
main = do
  putStrLn "hello world"

---------------------------
-- Simple Language
---------------------------
{-
data Expr = I Int
          | Add Expr Expr
          | Mul Expr Expr

eval :: Expr -> Int
eval (I x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- ghci> eval (Mul (I 5) (I 6))
-}

---------------------------
-- Extended Langauge, v1 Naive extension
---------------------------
    {-
data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr
          | Eq  Expr Expr

eval :: Expr -> Either Int Bool
eval (I x) = Left x
eval (B b) = Right b
eval (Add x y) = eval x + eval y -- ?

eval :: Expr -> Maybe (Either Int Bool) -- ?
-}
--------------------------- 
-- Extended Language, v2 Phantom types
---------------------------
{-
data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)

-- `a` is a phantom type, it tracks the expression type

-- Smart constructors

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

add :: Expr Int -> Expr Int -> Expr Int
add = Add

-- ghci> :t add (i 5) (i 6)
-- ghci> :t add (b 5) (i 6)

-- Now try and write eval function

--eval :: Expr a -> a
--eval (I n) = n

-- I 5 :: Expr String  is legal

-- need to restrict the return types of constructors
-}
------------------------------
-- Extended Language, v3 GADTs
------------------------------

{-
data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2
-}

---------------------------
-- Example, List
---------------------------

data List a = Nil 
            | Cons a (List a)

data List a where
    Nil  :: List a
    Cons :: a -> List a -> List a

data Empty
data NonEmpty

data List a e where
    Nil  :: List a Empty
    Cons :: a -> List a e -> List a NonEmpty

safeHead :: List a NonEmpty -> a
safeHead (Cons x _) = x

emptyOrUnit False = Nil
emptyOrUnit True  = Cons () Nil
-}
