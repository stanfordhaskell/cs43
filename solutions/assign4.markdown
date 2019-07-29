---
title: Assignment 4
---

# Solution

Exactly 6 lines of code needed to be modified to complete the assignment.
First, the `Expr` type needed to be modified to represent sums and products
of arbitrary numbers of expressions.

This:

```
data Expr = Num Int
          | Neg Expr
          | Add Expr Expr
          | Mul Expr Expr
```

becomes this:

```
data Expr = Num Int
          | Neg Expr
          | Add [Expr]
          | Mul [Expr]
```

Second, the `eval` function needs to be modified to evaluate `Add` and `Mul`.

This:

```
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
```

becomes this:

```
eval (Add xs) = sum $ map eval xs
eval (Mul xs) = product $ map eval xs
```

Finally, the parsers for `Add` and `Mul` need to be modified to parse multiple
expressions and return a list to the constructors (`Add` and `Mul` are now
no longer constructors that take two `Expr` values but that take a single value
of type `[Expr]`).

This:

```
add :: Parser Expr
add = Add <$> ((spaceChar '+') *> expr) <*> expr
```

becomes this:

```
add :: Parser Expr
add = Add <$> ((spaceChar '+') *> some expr)
```

and similarly for `mul`.
