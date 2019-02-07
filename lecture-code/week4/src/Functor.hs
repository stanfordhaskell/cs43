-- Functor: one of the most fundamental typeclasses
-- in standard library.  It represents a "container"
-- as well as an ability to apply functions to it.

-- f is a container
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) :: fmap . const

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap g (x:xs) = g x : fmap g xs
  -- alternatively: fmap = map.

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just a) = Just (g a)

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- NonEmpty a


-- fmap (+2) [1..10]
-- fmap (+2) Just 2
-- fmap (+2) Nothing
-- fmap (2*) (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4)))

-- Kind mismatch error!
-- instance Functor Int where fmap = map

-- Laws
-- Sort of like a group "homomorphism"
-- fmap id = id
-- fmap (g . h) = (fmap g) . (fmap h)
--
-- ghci> (fmap even . fmap length) (Just "twelve")
-- Just True
-- ghci> fmap (even . length) (Just "twelve")
-- Just True

-- Curry intuition
-- Does not really take two parameters, but takes a single
-- parameter and returns a function.
-- fmap :: (a -> b) -> (f a -> f b)
-- fmap "lifts" a function from the normal world into the "f" world.
--
-- Lifting (will come back with Monads)


-- This an important example.
-- What is fmap over functions?
-- In Control.Monad.Instances
instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))

-- OR
instance Functor ((->) r) where
  fmap = (.)

-- 
-- :m + Control.Monad.Instances
-- :t fmap (*3) (+100)
-- fmap (*3) (+100) 1


-- fmap :: (a -> b) -> (->) r a -> (->) r b

-- OR

-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
-- A value of type (e -> a) is an r-indexed container, with one value
-- of a for each value of r.

-- Mapping a function over every value corresponds to function copmposition.
-- We first apply r-> a function to pick aut an a from the original container
-- then apply the a -> b function to transform the element we picked.
--
-- Either e a is a container that can either contain a value of type a or a value of type e.



-- intro to containers.  From https://haskell-containers.readthedocs.io/en/latest/index.html
-- Data.Map.Strict; Data.Map.Lazy; Data.IntMap
-- import qualified Data.Map.Strict as Map
-- Strict version: not lazy
-- :t Map.fromList
-- Keys need to be in the Ord typeclass.
-- let m1 = Map.fromList [("a", 1), ("b", 2)]
-- let m2 = Map.delete "a" m1
-- All implementations are immutable.  Any update functions create a new map.
--
-- nums = Map.fromList [(1, "one"), (2, "two"), (3, "three")]
-- Map.lookup 3 nums
-- Map.lookup 4 nums
--
-- let moreNums = Map.insert 4 "four" nums
-- Map.member 4 moreNums
--
-- Intro to sequences.
--
-- Lists:
-- Lists have O(1) cons and pattern matching.
-- Similar to generators in Python (which are also lazy).
-- !!, ++ is O(N)
--
-- Sequences work very similar to lists, with a few key differences.
-- Functions are similar.
-- Sequences based on finger trees.
-- O(1) access to front and rear with <|, |>, viewl, viewr.
-- O(log N) concatenation with ><
-- O(log N) splitting with splitAt, take, and drop
-- O(log N) access to any elemeent with lookup, !?, etc.

-- Performance notes: https://wiki.haskell.org/Performance

--  import Data.Sequence (Seq(..), (<|), (|>), (<>))
--  import qualified Data.Sequence as Seq
--  let nums = Seq.fromList [1,2,3]
--  0 <| nums
--  nums |> 4
--  Seq.reverse (Seq.fromList [0, 1, 2])
--  Seq.fromList [-2, -1] >< nums
--  Seq.length nums
--  Seq.lookup 2 nums vs. Seq.index 2 nums

