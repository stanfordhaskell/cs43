--> (+ 1) $ 5
--> fmap (+ 1) $ Just 5
--> (+ 1) <$> Just 5

-- alternative interpretation, contex to which functions can be applied

--> :t ($)
--> :t (<$>)






--> (+) 5 6

-- how to write (+) (Just 5) (Just 6) ?

--> (+) <$> (Just 5) <$> (Just 6)
--> :t (+)
--> :t ((+) <$>)
--> :t (+) <$> (Just 5)
--> :t (Just 6)

--> :t (<$>)
--> :t (<*>)

--> (+) <$> (Just 5) <*> (Just 6)







-- Applicative Functors

class (Functor f) => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
  -- ... second function


instance Applicative Maybe where
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _        = Nothing
  -- ... second function



--> (+) <$> (Just 5) <*> (Just 6)
--> (+) <$> (Just 5) <*> Nothing
--> (+) <$> Nothing <*> (Just 6)









-- Applicative Functors - full

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b


instance Applicative Maybe where
  pure         = Just
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _        = Nothing


--> pure 5 :: Maybe Int
--> (+) <$> (Just 5) <*> (Just 6)
--> pure (+) <*> (Just 5) <*> (Just 6)








-- Laws  - don't worry too much right now

pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

f <$> x = pure f <*> x                      -- follows from above and fmap laws






-- functions operating on applicatives

liftA2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 f x y = f <$> x <*> y

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f x y z = f <$> x <*> y <*> z









-- Example of "contextual application"


type Name = String

data Employee = Employee { name  :: Name
                         , phone :: String }
                deriving (Show)

Employee :: Name -> String -> Employee



(Name -> String -> Employee) ->
 (Maybe Name -> Maybe String -> Maybe Employee) -- optionality

(Name -> String -> Employee) ->
 ([Name] -> [String] -> [Employee]) -- two interpretations, non-determinism and zip

(Name -> String -> Employee) ->
 ((e -> Name) -> (e -> String) -> (e -> Employee)) -- reader



-- what function do we need in all 3 cases?
















liftA2 Employee (Just "Mark") (Nothing)

liftA2 Employee ["Mark", "Maya"] ["777-7777", "888-8888", "999-9999"]

liftA2 Employee (ZipList ["Mark", "Maya"]) (ZipList ["777-7777", "888-8888", "999-9999"])

getEmployee = liftA2 Employee getName getNumber -- definition of getEmployee

e = database

getName e -> "Joe"
getNumber e -> "2222"







{-- List Instances

instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]
 
  (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [ g x | g <- gs, x <- xs ]



newtype ZipList a = ZipList { getZipList :: [a] }
 
instance Applicative ZipList where
  pure :: a -> ZipList a
  pure = ZipList (repeat x)
 
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

--}
