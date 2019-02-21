-- Note does not actually compile

module Code where

--------------------------
-- Example 1: Maybe
--------------------------

data Maybe a = Just a | Nothing

instance Functor Maybe where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

instance Applicative Maybe where
  pure = Just

  Just f  <*> m       = fmap f m
  Nothing <*> _m      = Nothing


instance Monad Maybe where
  (Just x) >>= f = f x
  Nothing  >>= _ = Nothing

-------------------------
-- Do notation
-------------------------

do { x } = x
 
do { x ; <stmts> }
  = x >> do { <stmts> }
 
do { v <- x ; <stmts> }
  = x >>= \v -> do { <stmts> }
 
do { let <decls> ; <stmts> }
  = let <decls> in do { <stmts> }

--------------------------------
-- Example 2: []
--------------------------------

instance Functor [] where
    fmap = map

instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Monad []  where
  xs >>= f             = [y | x <- xs, y <- f x]
  (>>) = (*>)

-------------------------------
-- MonadPlus
-------------------------------

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mzero = empty

  mplus :: m a -> m a -> m a
  mplus = <|>

-- laws (from alternative)
mzero `mplus` m  =  m
m `mplus` mzero  =  m
m `mplus` (n `mplus` o)  =  (m `mplus` n) `mplus` o

-- additional laws
mzero >>= f  =  mzero -- left zero
m >> mzero   =  mzero -- right zero

-------------------------
-- Example 3: IO
-------------------------


>>= :: m a -> (a -> m b) -> m b

>>= :: IO String -> (String -> IO ()) -> IO ()


--------------------------
-- Example 4a: Reader
--------------------------

instance Functor ((->) e) where
 fmap = (.)

instance Applicative ((->) e) where
  pure x = (\_ -> x)  
  f <*> g = \x -> f x (g x)  

instance Monad ((->) r) where  
  h >>= f = \w -> f (h w) w 

--------------------------
-- Example 4b: Reader
--------------------------

newtype Reader r a = Reader { runReader :: r -> a }

-- instances similar to as above


-------------------------
-- Example 5: Writer
-------------------------

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where  
  return x = Writer (x, mempty)  -- pure from Applicative
  (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v <> v')  


--------------------------
-- Example 6: State
--------------------------

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  
