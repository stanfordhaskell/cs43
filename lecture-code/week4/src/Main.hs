-- Functor: a typeclass representing
-- "types that can be mapped over."

-- List, Map, Tree are instances of Functor.

-- f is a "container"

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap g (x:xs) = g x : fmap g xs
  -- alternatively: fmap = map.

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just a) = Just (g a)

{-instance Functor Either e where-}
  {-fmap :: (a -> b) -> Either e a -> Either e b-}

instance Functor Tree
  ...

instance Functor Map
  ...

instance Functor Sequence
  ...

-- Laws
-- Sort of like a group "homomorphism"
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)

-- Curry intuition
-- Does not really take two parameters, but takes a single
-- parameter and returns a function.
-- fmap :: (a -> b) -> (f a -> f b)
-- fmap "lifts" a function from the normal world into the "f" world.

-- Monoid: output is independent of the input. 
-- For example: multiplying 
