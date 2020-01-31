# Discussion of stack

(This is kind of covered elsewhere).

# Review of applicative

```
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

# Alternative

We will introduce the `Alternative` typeclass, which is described here: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#g:2

```
 class Applicative f => Alternative f where
   empty :: f a
   (<|>) :: f a -> f a -> f a
```

In the case of Maybe, the intuition is:

- Take the first Just.

In the case of the list type, the associative operation is concatenation.

# Parsers

We will start with a datatype for parsers, namely

```
newtype Parser a = P { getParser :: (String -> Maybe (a,String)) }
```

We will start with a simple parser that captures a character from a string.  We have

```
item :: Parser Char
item = P $ \case
  []  -> Nothing
  (x:xs) -> Just (x, xs)
```

And if we run
```
getParser item "hello"
```
We get
```
Just ('h', "ello")
```

We can write a functor instance for our parser.  Essentially, it applies a function `f` to the resulting value from a parse.

```
instance Functor Parser where
  fmap f p = P $ \inp -> case getParser p inp of
    Nothing       -> Nothing
    Just (v, out) -> Just (f v, out)
```

So if we run
```
getParser (fmap toUpper item) "hello"
> Just ('H', "ello")
```

Thanks to the functor instance, we can write an applicative instance:

```
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \inp -> Just (x, inp)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \inp -> case getParser pf inp of
    Nothing -> Nothing
    Just (f, tmp) -> getParser (fmap f px) tmp
```

When do we get a Parser (a -> b)?  One situation in which this may happen is when we fmap a two argument function over a Parser.

```
instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \inp -> case getParser p inp of
    Nothing -> getParser q inp
    r -> r
```

These typeclasses allow us to make even more parsers:
```
:t (,) <$> item <*> item
> (,) <$> item <*> item :: Parser (Char, Char)
```
Applying this,
```
getParser ((,) <$> item <*> item) "hello"
> Just (('h', 'e'), "llo")
```

We can write functions that generate parametrized parsers.  For instance, suppose we want a parser that captures a character if it satisfies a predicate.
```
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = P $ \case
  x:xs | pred x -> Just (x, xs)
  _ -> Nothing
```

We can write parsers for specific characters and "not" those characters:
```
char :: Char -> Parser Char
char = satisfy . (==)  -- char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar = satisfy . (/=)
```

We can write a digit parser that will be used for our calculator:

```
digit = Parser Char
digit = satisfy isDigit
```

And finally, a parser that parses spaces:
```
spaces :: Parser String
spaces = many $ char ' '
```

## Writing a calculator

Now we will proceed to write our calculator app.  We'll start by defining a type that models expressions:

```
data Expr = Num Int
          | Neg Expr
          | Add Expr Expr
          | Mul Expr Expr
```

And we can write a function `eval` that goes from `Expr -> Int`

```
eval :: Expr -> Int
eval (Num x) = x
eval (Neg x) = - eval x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
```

We can write a function that parses a character optionally surrounded by spaces.
```
spaceChar :: Char -> Parser Char
spaceChar c = spaces *> (char c) <* spaces
```

Next, we parse an expression in parentheses (optionally surrounded by spaces):
```
inParens :: Parser a -> Parser a
inParens p = spaces *> char '(' *> p <* char ')' <* spaces
```

Now we can write parsers in our language.  We'll start with `number :: Parser Expr`

```
number :: Parser Expr
number = Num . read <$> (spaces *> some digit <* spaces)
```

We can do the same thing for all other expressions:

```
neg :: Parser Expr
neg = (spaceChar '-') *> (Neg <$> expr)

add :: Parser Expr
add = (spaceChar '+') *> (Add <$> expr <*> expr)

mul :: Parser Expr
mul = (spaceChar '*') *> (Mul <$> expr <*> expr)
```

Now we will write an expression parser
```
expr = number <|> inParens (neg <|> add <|> mul)
```

And finally, we'll write a function `interpret` that takes a String -> Maybe Int
```
interpret :: String -> Maybe Int
interpret s = fmap (eval . fst) $ getParser expr s

calculator :: String -> String
calculator = show . interpret
```

And we're done:
```
interpret "(+ 1 (* 3 4))"
```

