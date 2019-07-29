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

a = True
-- a = False -- immutable

b = c + 1
c = 1 -- order invariant

-- Functions -----------------------------------

inc x = 1 + x

inc' x = (+) 1 x 

-- > inc 4


absMax x y = if abs x > abs y
                then abs x
                else abs y

absMax' x y
    | abs x > abs y = abs x
    | otherwise     = abs y -- otherwise == True

-- > absMax (-5) 2


applyTwice f x = f (f x)

applyTwice' f x = f $ f x

-- > applyTwice inc 4

-- Anonymous Functions --------------------------------

inc'' = \x -> x + 1
absMax'' = \x y -> if abs x > abs y then abs x else abs y
applyTwice'' = \f x -> f (f x)


------------------------------------
-- Every expression/value has a type
------------------------------------

-- > :t True
-- > :t 1 
-- > :t (1 :: Int)
-- > :t (1 :: Float)


-- > :t not
-- > not (1 :: Int)

-- Inference
-- > :t inc 
-- > :t (inc :: Int -> Int)
-- > (inc :: Int -> Int) (1 :: Float)

incInt :: Int -> Int
incInt x = x + 1

-- Currying ----------------------------

sumInts x y = (x :: Int) + y

-- > :t sumInts
-- > sumInts 1 2
-- > (sumInts 1) 2

sumInts' :: Int -> (Int -> Int)
sumInts' = \x -> (\y -> x + y)

incInt' :: Int -> Int          -- read type signature as take and give
incInt' x = sumInts 1 x

incInt'' :: Int -> Int         -- read type signature as value
incInt'' = \x -> (sumInts 1) x -- applies function to x

incInt''' :: Int -> Int
incInt''' = sumInts 1



lowestForm :: Int -> Int -> Bool
lowestForm num denom
    | denom == 0         = error "error, divide by 0"
    | gcd num denom /= 1 = False
    | otherwise          = True

-- try and replace the second False with "invalid fraction"
--
-- need type for errors / multiple possibilities since errors
-- are not supported


-----------------------------------
-- Algebraic Datatypes
-----------------------------------
    --

data Person = Student String Int Float
            | Teacher String Int
    deriving (Show)

-- need to build up more complicated types

-- data Datatype = Constructor Type Type ...
--               | Constructor 
--               ...
--               | Constructor Type Type ...

data Point = Point Float Float
    deriving (Show)




getName :: Person -> String
getName (Student name _ _) = name
getName (Teacher name _ ) = name

-- > Point 0 1
-- > :t Point 0 1
-- > :t Point

norm :: Point -> Float
norm (Point x y) = sqrt $ x ^ 2 + y ^ 2

xCoord (Point x _) = x
yCoord (Point _ y) = y

data Point' = Point' { xCoord' :: Float,
                       yCoord' :: Float }




data Rect = Rect2D Float Float
          | Rect3D Float Float Float
    deriving (Show)

rectLength (Rect2D l _)   = l
rectLength (Rect3D l _ _) = l

rectWidth  (Rect2D _ w)   = w
rectWidth  (Rect3D _ w _) = w

rectHeight  (Rect2D _ _)   = 0 -- Does this make sense?
rectHeight  (Rect3D _ _ h) = h

-- need type for optional or missing values


-- data Maybe a = Just a | Nothing
data OptionalFloat = Existing Float | Nope

rectHeight'  (Rect2D _ _)   = Nothing 
-- rectHeight'  (Rect3D _ _ h) = Just h




-- above, we needed a type for alternate or error

-- data Either a b  =  Left a | Right b

lowestForm' :: Int -> Int -> Either String Bool
lowestForm' num denom
    | gcd num denom /= 1 = Right False
    | denom == 0         = Left "error, divide by 0"
    | otherwise          = Right True


-- Recursive datatypes  ----------------------

data IntList = IntNil
             | IntCons Int IntList
    deriving (Show)

-- > IntNil
-- > IntCons 1 IntNil
-- > IntCons 2 (IntCons 1 IntNil)
-- > IntCons 2 $ IntCons 1 IntNil

intListLength :: IntList -> Int
intListLength IntNil = 0
intListLength (IntCons x xs) = 1 + intListLength xs

-- > intListLength $ IntCons 2 $ IntCons 1 IntNil

data List a = Nil
            | Cons a (List a)
    deriving (Show)

listLength :: List a -> Int
listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs


data IntBinTree = IntEmpty
                | IntNode Int IntBinTree IntBinTree
    deriving (Show)

intBinTreeSize :: IntBinTree -> Int
intBinTreeSize IntEmpty = 0
intBinTreeSize (IntNode x l r) = 1 + lsize + rsize
  where lsize = intBinTreeSize l
        rsize = intBinTreeSize r
                                 

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  









-- > Nil
-- > :t Nil
-- > Cons True Nil
-- > :t Cons True Nil
-- > Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil
-- > :t Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil

listHead :: List a -> a
listHead (Cons a _) = a

-- > listHead (Cons 1 $ Cons 2 $ Cons 3 Nil)
-- > listHead Nil

safeListHead :: List a -> Maybe a
safeListHead Nil        = Nothing
safeListHead (Cons a _) = Just a

{- Lists in Haskell

data [] a = []
          | a : [a]

head (x:xs)             =  x
-}

-- > [1, 2, 3] == 1 : 2 : 3 : []

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- > safeHead [1, 2, 3, 4]
