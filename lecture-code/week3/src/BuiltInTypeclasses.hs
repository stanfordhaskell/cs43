---------------------------------------------
-- 1. Eq: defines equality and inequality. --
---------------------------------------------

class  Eq a  where  
  (==), (/=)  ::  a -> a -> Bool  

  x /= y  = not (x == y)  
  x == y  = not (x /= y)

-- Minimum complete definition: (==) OR (/=)

-- GHCI examples:

-- :t (==)
-- :t (/=)
-- 2 == 3
-- 3 == 3

-- Example instance...
data Color = Red | Green | Blue

instance Eq Color where
  (==) Red Red     = True
  (==) Green Green = True  
  (==) Blue Blue   = True
  (==) _    _      = False

-- Alternatively, use automatic derivation!

-- Say: how haskell derives the equality operator
-- Todo: look through ordering derivation
data Color2 = Red2 | Green2 | Blue2
  deriving (Eq)

------------------------------------------------
-- 2. Ord: defines functions that compare values.
------------------------------------------------

-- Built in ordering type:
data Ordering = LT | EQ | GT

-- Typeclass definition.
class  (Eq a) => Ord a  where
  compare              :: a -> a -> Ordering  
  (<), (<=), (>=), (>) :: a -> a -> Bool  
  max, min             :: a -> a -> a 

  compare x y | x == y    = EQ  -- a must have implemented ==
              | x <= y    = LT  
              | otherwise = GT  

  x <= y  = compare x y /= GT  
  x <  y  = compare x y == LT  
  x >= y  = compare x y /= LT  
  x >  y  = compare x y == GT  

  max x y | x <= y    =  y  
          | otherwise =  x  
  min x y | x <= y    =  x  
          | otherwise =  y

-- Minimum complete definition:
-- compare OR <=

-- GHCI examples
-- :t compare
-- :t (<=)
-- compare 2.0 2.1
-- compare 'a' 'b'
-- 2.0 <= 2.1
-- Hoogle Ord
-- Look at laws of ORD.
-- Hoogle: Ord a => [a] -> [a]

-- Example instance
--
-- By hand: write instnaces for Eq / Ord / Show
instance Ord Color where
  compare Red Red = EQ
  compare Green Green = EQ
  compare Blue Blue = EQ
  compare Red Green = LT
  compare Red Blue = LT
  compare Green Blue = LT
  compare _     _    = GT
  -- ...

-- Deriving Ord works here.

-------------------------------------
-- 3. Num: the basic numeric typeclass
-------------------------------------

class  Num a  where
    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    fromInteger         :: Integer -> a

    x - y               = x + negate y
    negate x            = 0 - x

-- Minimal complete definition: (+), (*), abs, signum, fromInteger, and negate (or (-)).

-----------------------------------------------------
-- 4. Enum: operations on sequentially ordered types.
-----------------------------------------------------

class  Enum a  where  
    succ, pred     :: a -> a   -- Successor / predecessor of a value
    toEnum         :: Int -> a            -- Convert from an Int
    fromEnum       :: a -> Int            -- Convert to an Int 
    enumFrom       :: a -> [a]            -- [n..]
    enumFromThen   :: a -> a -> [a]       -- [n,n'..]  
    enumFromTo     :: a -> a -> [a]       -- [n..m]  
    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]  

    succ                   = toEnum . (+ 1)  . fromEnum
    pred                   = toEnum . (subtract 1) . fromEnum
    enumFrom x             = map toEnum [fromEnum x ..]
    enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
    enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

-- Minimum complete definition:	
-- toEnum and fromEnum

-- Example instance:
data Color = Red | Green | Blue
  deriving (Read, Show, Eq, Ord, Enum)

-- TODO: add example Red .. Blue 

-- GHCI examples
-- :t toEnum
-- :t fromEnum
-- take 10 $ enumFromThen 2 4
-- take 10 $ enumFromTo 1 100

----------------------------------------------------------------
-- 5. Read and Show: converting strings to values and vice versa
----------------------------------------------------------------

-- This is the machinery we invoke when we type
-- "deriving Show" after declaring a custom type.
-- A little complicated, and the details don't matter too much.
-- TODO: add examples on read and show when they're introduced.
-- omit read from here; but talk about show.

type  ShowS   = String -> String  
 
class  Show a  where  
    showsPrec :: Int -> a -> ShowS  
    show      :: a -> String  
    showList  :: [a] -> ShowS  
 
    showsPrec _ x s   = show x ++ s  
    show x            = showsPrec 0 x ""  
    -- ... default decl for showList given in Prelude

-- Minimal complete definition: showsPrec or show


-- Other important typeclasses
-- Integral (Int, Integer)
-- Fractional (Double)
-- Floating (Double)
-- RealFrac (round, truncate)
