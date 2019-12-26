module Main where

import Control.DeepSeq
import Control.Parallel.Strategies hiding (parPair, evalList, rdeepseq)

main :: IO ()
main = do
  putStrLn "hello world"

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-------------------------------------------
-- Lazy Evaluation
-------------------------------------------

-- Non-strict vs Lazy evaluation

-- ghci> let x = 3 :: Int
-- ghci> let x = 1 + 2 :: Int
-- ghci> let z = (x, x)
-- ghci> let z = (x, x + 1)
-- ghci> let xs = map (+ 1) [1..10] :: [Int]


-------------------------------------------
-- Eval Monad
-------------------------------------------

{-
data Eval a
instance Monad Eval

runEval :: Eval a -> a

rpar :: a -> Eval a
rseq :: a -> Eval a
-}

-- TODO rpar
-- TODO sudoku

-------------------------------------------
-- NFData
-------------------------------------------

{-
class NFData a where
  rnf :: a -> ()
  rnf a = a `seq` ()

instance NFData Bool
-}

data Tree a = Empty | Branch (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf Empty = ()
  rnf (Branch l a r) = rnf l `seq` rnf a `seq` rnf r


{-
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

force :: NFData a => a -> a
force x = x `deepseq` x
-}

-------------------------------------------
-- Evaluation Strategies
-------------------------------------------

-- type Strategy a = a -> Eval a

parPair :: Strategy (a,b)
parPair (a,b) = do
  a' <- rpar a
  b' <- rpar b
  return (a',b')

-- ghci> (fib 17, fib 18)
-- ghci> runEval (parPair (fib 17, fib 18))

{-
using :: a -> Strategy a -> a
x `using` s = runEval (s x)
-}

-- ghci> (fib 17, fib 18) `using` parPair


evalPair :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair sa sb (a,b) = do
  a' <- sa a
  b' <- sb b
  return (a',b')

parPair' :: Strategy (a,b)
parPair' = evalPair rpar rpar

-- fully evaluates arguments
rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)

-- wrap strategy in rpar
-- rparWith :: Strategy a -> Strategy a

parPair'' :: Strategy a -> Strategy b -> Strategy (a,b)
parPair'' sa sb = evalPair (rparWith sa) (rparWith sb)

deepParPair :: (NFData a, NFData b) => Strategy (a,b)
deepParPair = parPair'' rdeepseq rdeepseq 


evalList :: Strategy a -> Strategy [a]
evalList strat []     = return []
evalList strat (x:xs) = do
  x'  <- strat x
  xs' <- evalList strat xs
  return (x':xs')

parList :: Strategy a -> Strategy [a]
parList strat = evalList (rparWith strat)


