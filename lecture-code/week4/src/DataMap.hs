import qualified Data.Map as M

class Functor' f where
  myfmap :: (a -> b) -> f a -> f b

instance (Ord k) => Functor' (M.Map k) where
  fmap' f = M.fromList . map (second f) . M.toList
