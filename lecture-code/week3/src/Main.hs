module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Suppose we define a new type, Color.
data Color = Red | Green | Blue

-- (==) 
-- We might want to define an equality check for it.
colorEq :: Color -> Color -> Bool
colorEq Red Red     = True
colorEq Green Green = True
colorEq Blue Blue   = True
colorEq _    _      = False

-- Suppose that (==) didn't exist.
-- We might implement a function, stringEq as follows.
stringEq :: [Char] -> [Char] -> Bool

stringEq [] []         = True
stringEq (x:xs) (y:ys) = (x == y) && stringEq xs ys

stringEq _ _ = False

-- But this is super inefficient - we have to write separate functions
-- for each notion of "equality" that we want to encode.

-- Introducing typeclasses:
-- This allows us to give different definitions of a function
-- for different types.
-- These are not classes, similar to the notion of _interfaces_ in Java.

-- Example typeclass: BasicEq
class BasicEq a where
  isEqual :: a -> a -> Bool

-- Example _instance_ of the BasicEq typeclass on booleans.
instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _     _     = False

-- isEqual True True now works
-- isEqual "Hi" "Hi2" will not

-- Expanding on this: we can define a more sophisticated typeclass
-- with multiple functions.  For a type to be in the "BasicEq2" type class,
-- it must implement _one of_ "isEqual2" and "isNotEqual2".

-- Need to put types for all functions
-- But the implementations are optional
class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isEqual2 x y = not (isNotEqual2 x y)

  isNotEqual2 :: a -> a -> Bool
  isNotEqual2 x y = not (isEqual2 x y)

-- We can make `Color` an instance of 
-- `BasicEq2` by implementing `isEqual2`.
-- Compiler is really smart, it will figure out the other.
-- everything to left of => is a type constraint
instance BasicEq2 Color where
  isEqual2 Red Red     = True
  isEqual2 Green Green = True
  isEqual2 Blue Blue   = True
  isEqual2 _    _      = False

-- Want to spend time 
-- Look at source of Eq
-- :info
-- Look at docs

-- Look at Show
-- :t show
instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue" 

-- Automatic typeclass derivation!
-- Ord: typeclass for ordering
-- Read: "inverse" of Show
data Color2 = Red2 | Green2 | Blue2
  deriving (Read, Show, Eq, Ord)










