module Main where

main :: IO ()
main = putStrLn "hello world"

---------------------------------
---------------------------------
-- EXPRESSIONS, VALUES, AND TYPES
---------------------------------
---------------------------------

-------------------------------
-- Every expression has a value
-------------------------------

a = 1
-- a = 3 -- immutable

b = (c, 5, "This is a tuple")
c = True -- order invariant

d = [1, 2, 3, 4]
-- > [1..4]
-- > take 4 [1..]

e = [5 * x | x <- [1..4],
             mod x 3 == 0 ] -- infinite values
-- > take 5 e

f x = if x > 10
         then 10
         else x

f' x
    | x > 10 = 10
    | True   = x

g x y = x * y + 1
-- > g 1 2 == (g 1) 2
-- > g 1 2 == 1 `g` 2
-- > 1 + 2 == (+) 1 2

h x y z
    | x < y && y < z = "ascending"
    | x > y && y > z = "descending"
    | otherwise      = "z wins!"

-- > otherwise == True


------------------------------------
-- Every expression/value has a type
------------------------------------

-- > :t True
-- > :t 1 
-- > :t (1 :: Int)
-- > :t (True, False, 1 :: Int)
-- > :t "Hello"
-- > :t [1, 2, 3, 15]
-- > :t ("hello" :: String) == ("hello" :: [Char])

type StringAlias = String

-- > :t (1 :: Int) == (1 :: Integer)


-- > :t not
-- > :t (&&)
-- > :t ((&&) True)
-- > :t (+)
-- > :t ((+) :: Int -> Int -> Int)
--
sum3 x y z = x + y + z

inc :: Int -> Int  -- inference
inc x = ((+) 1) x

inc' :: Int -> Int
inc' = (+) 1

lowestForm :: Int -> Int -> Bool
lowestForm num denom
    | gcd num denom /= 1 = False
    | denom == 0         = False
    | otherwise          = True

-- try and replace the second False with "invalid fraction"
--
-- need type for errors / multiple possibilities since errors
-- are not supported


-----------------------------------
-- Algebraic Datatypes
-----------------------------------

-- need to build up more complicated types

-- data Datatype = Constructor Type Type ...
--               | Constructor 
--               ...
--               | Constructor Type Type ...

data Point = Point Float Float

data Coord = Point0D
           | Point1D Float
           | Point2D Float Float
           | Point3D Float Float Float
    deriving (Eq, Show)

getXCoord :: Coord -> Float
getXCoord (Point3D x y z) = x
getXCoord (Point2D x y)   = x
getXCoord (Point1D x)     = x
getXCoord Point0D         = 0.0

-- need type for optional or missing values


-- data Maybe a = Just a | Nothing

getXCoord' :: Coord -> Maybe Float
getXCoord' (Point3D x y z) = Just x
getXCoord' (Point2D x y)   = Just x
getXCoord' (Point1D x)     = Just x
getXCoord' Point0D         = Nothing


-- data Either a b  =  Left a | Right b

lowestForm' :: Int -> Int -> Either String Bool
lowestForm' num denom
    | gcd num denom /= 1 = Right False
    | denom == 0         = Left "error, divide by 0"
    | otherwise          = Right True

