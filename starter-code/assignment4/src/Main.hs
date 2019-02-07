module Main where

import Control.Applicative
import Parser (Parser(..), item, satisfy, char, notChar, digit, spaces)

main :: IO ()
main = putStrLn "hi"


-- The goal of this assignment is to extend an interpreter for a simple integer
-- calculator language. We define our initial grammer as follows. An expression
-- can be either an integer or
--   (- expr)
--   (+ expr expr2)
--   (* expr expr2)
-- where expr and expr2 are expressions. This gives expressions that look like
--   (+ (- 3) (* 4 (+ 5 6)))
-- Further, we want to allow arbitrary numbers of spaces between and around
-- expressions.


-- First we define a datatype the encodes expressions in our language.
-- Our goal will be to write a parser that transforms a string into
-- values of type Expr.

data Expr = Num Int
          | Neg Expr
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)



-- We also have an evaluator for `Expr` values.

eval :: Expr -> Int
eval (Num a) = a
eval (Neg a) = - eval a
eval (Add as) = eval l + eval r
eval (Mul as) = eval l * eval r

-- ghci> eval (Add (Mul (Num 4) (Num 5)) (Neg (Num 4)))



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
add = Add <$> ((spaceChar '+') *> expr) <*> expr

mul :: Parser Expr
mul = Mul <$> ((spaceChar '*') *> expr) <*> expr

-- ghci> getParser number " 4444  "
-- ghci> getParser neg " -  555"
-- ghci> getParser add " +666 555"

expr = number <|> inParens (neg <|> add <|> mul)

-- ghci> getParser expr "(+ (* 2 3) (- 5))"
-- ghci> getParser expr " (+(*2 3)(-5))  "
-- ghci> getParser expr " (+(*23)(-5))  "

