module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Problem 1 --

-- MyEither used to distinguish from Prelude.Either
data MyEither a b = Left a | Right b

instance Functor (MyEither a) where
  fmap = undefined -- Implementation here

-- Problem 2 --
data Tree a = Node a [Tree a]

instance Functor Tree where
  fmap = undefined -- Implementation here
