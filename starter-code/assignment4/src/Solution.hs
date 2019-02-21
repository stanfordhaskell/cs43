module Main where

import Control.Applicative
import Parser (Parser(..), item, satisfy, char, notChar, digit, spaces)

main :: IO ()
main = putStrLn "hi"


-- First we define a datatype the encodes expressions in our language.
-- Our goal will be to write a parser that transforms a string into
-- values of type Expr.

data Expr = Num Int
          | Neg Expr
          | Add [Expr]
          | Mul [Expr]
          deriving (Show)


-- We also have an evaluator for `Expr` values.

eval :: Expr -> Int
eval (Num x) = x
eval (Neg x) = - eval x
eval (Add xs) = sum $ map eval xs
eval (Mul xs) = product $ map eval xs

-- ghci> eval $ Add [Mul [(Num 4), (Num 5), (Num 6)], (Neg (Num 4))]


-- first we write two helper parsers

-- parses a character optionally surrounded by spaces
spaceChar :: Char -> Parser Char
spaceChar c = spaces *> (char c) <* spaces

-- ghci> getParser (spaceChar 'a') "  a    "

-- parses an expression in parenthesis (optionally surrounded by spaces)
inParens :: Parser a -> Parser a
inParens p = spaces *> char '(' *> p <* char ')' <* spaces

-- ghci> getParser (inParens spaceChar) "   (   d ) ef"



-- now we can write parsers for expressions in our language

number :: Parser Expr
number = Num . read <$> (spaces *> some digit <* spaces)

neg :: Parser Expr
neg = Neg <$> ((spaceChar '-') *> expr)

add :: Parser Expr
add = Add <$> ((spaceChar '+') *> some expr)

mul :: Parser Expr
mul = Mul <$> ((spaceChar '*') *> some expr)

-- ghci> getParser number " 4444  "
-- ghci> getParser neg " -  555"
-- ghci> getParser add " +666 555"

expr = number <|> inParens (neg <|> add <|> mul)

-- ghci> getParser expr "(+ (* 2 3) (- 5))"
-- ghci> getParser expr " (+(*2 3)(-5))  "
-- ghci> getParser expr " (+(*23)(-5))  "

interpret :: String -> Maybe Int
interpret s = case getParser expr s of
                []     -> Nothing
                (x:xs) -> Just $ eval $ fst x

-- ghci> interpret "(+ 1 (* 3 4))"
-- ghci> interpret "(+ 1 (* 3 4)"

