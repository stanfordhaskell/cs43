module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Example eq function.
-- Source: RWH, Chapter 6
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red     = True
colorEq Green Green = True
colorEq Blue Blue   = True
colorEq _    _      = False

-- Suppose we wrote a function to test for equality on strings.
stringEq :: [Char] -> [Char] -> Bool

stringEq [] []         = True
stringEq (x:xs) (y:ys) = (x == y) && stringEq xs ys

stringEq _ _ = False

-- Example typeclasses
class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _     _     = False

-- isEqual True True now works
-- isEqual "Hi" "Hi2" will not
--

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isEqual2 x y = not (isNotEqual2 x y)

  isNotEqual2 :: a -> a -> Bool
  isNotEqual2 x y = not (isEqual2 x y)

-- Pattern match on constructors
instance BasicEq2 Color where
  isEqual2 Red Red     = True
  isEqual2 Green Green = True
  isEqual2 Blue Blue   = True
  isEqual2 _    _      = False

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
data Color2 = Red2 | Green2 | Blue2
  deriving (Read, Show, Eq, Ord)

-- Show that eq is automatically derived.

