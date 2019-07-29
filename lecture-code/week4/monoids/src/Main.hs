module Main where

import Data.List
import Data.Ord
import Data.Semigroup
import qualified Data.List.NonEmpty as NE
import Data.Coerce

main :: IO ()
main = do
  putStrLn "hello world"



-----------------------
-- background
-----------------------

{-- review of algebraic data types

data Typename a b c = Constructor 
                    | Constructor Type Type ...
                    | Constructor a b ...
* -> * -> * -> *
:k Tyname Int Bool Bool


-- docs example Maybe
data Maybe a = Nothing | Just a

data Typename a b c = T a b c  
-- customary Typename = Constructor

getFirst (T x _ _) = x
getSecond (T _ y _) = y
getThird (T _ _ z) = z

data Typename a b c = Constructor { getFirst :: a
                                  , getSecond :: b
                                  , getThird :: c }

newtype Typename a = Constructor a

getTypename (Typename a) = a

newtype Typename a = Typename { getTypename :: a }

-- docs examples Any, Sum

--}











{-- coerce

-- show docs

-- > (coerce $ Any True) :: Bool
-- > (coerce True) :: Any
-- > (coerce not :: Any -> Any) $ Any True

--}









{-- type of non-empty lists

data NonEmpty a = NonEmpty a [a]

(<|) :: a -> NonEmpty a -> NonEmpty a
a <| ~(b :| bs) = a :| b : bs

head :: NonEmpty a -> a
head ~(a :| _) = a

--}










{-- Semigroup

class Semigroup a where
  (<>) :: a -> a -> a
 
  sconcat :: NonEmpty a -> a
  sconcat = sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b
 
  stimes :: Integral b => b -> a -> a
  stimes = ...

-- see docs for laws

--}












{-- Monoid

class Semigroup a => Monoid a where
  mappend :: a -> a -> a
  mappend = (<>)

  mempty  :: a
 
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

-- see docs for laws

--}




--------------------
-- Example Instances
--------------------

-- Semigroup docs
-- Monoid docs for lifting semigroup to monoid


--------------------
-- Examples
--------------------

-- diagrams

-- functions

-- > :t compare
-- > compare 'a' 'a'
-- > compare 'a' 'b'
-- > compare "aaa" "baa" -- derivied list instance
-- > compare "aab" "aaa" -- compares characters sequentially
-- > compare "aa" "aaa"  -- shorter wins
-- > compare "ba" "aaa"  -- but letter counts first

-- > :t comparing
-- > :t (comparing length)
-- > (comparing length) "aaa" "aa" -- shorter wins
-- > (comparing length) "aa" "ab"  -- contents don't matter

-- revisit function instance
-- > :t (comparing length <> compare)
-- > (comparing length <> compare) "a" "aa"
-- > (comparing length <> compare) "a" "aa"
