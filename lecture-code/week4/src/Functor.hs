-- Functor: a typeclass representing
-- "types that can be mapped over."

-- List, Map, Tree are instances of Functor.

-- f is a type _function_, * -> *
class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b
  
  (<$) :: a        -> f b -> f a
  (<$) :: fmap . const

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x:xs) = g x : fmap g xs
  
