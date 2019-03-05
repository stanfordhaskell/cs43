---
title: Assignment 6
---

## Problem

This week's homework is to extend a simple language and evaluator we saw in
class. The starter code can be found [here](https://gitlab.com/stanford-lambda/stanford-lambda.gitlab.io/tree/master/starter-code/assignment6).

The first extension requires adding a two constructors to the `Expr` GADT, with
corresponding extensions to the `eval` function. An `O` constructor should
construct an expression out of a value of type `Ordering` (this is the type with
three values `LT`, `EQ`, and `GT`), and a `Comp` constructor shuld take two
expressions containing values in the `Ord` typeclass and return an ordering
expression.

The second extension requires adding a single `If` constructor, that takes a
boolean expression and two other expressions. The eval function should evaluate
the boolean expression and return the value of one of the other two expressions
depending on the result.

Finally, extend the Expr type and eval function to include one or more
additional expressions of your choice. For example, you might try to

1. add strings and a `Print` expression

1. add complex numbers and extend arithmatic to work on them

1. embed functions in the language with function and function application
   expressions

## Submission instructions

Send an email to cs43-win1819-staff@lists.stanford.edu with either:

- (Preferred) A link to a Gitlab / Github repository with your code.

- A .zip file with your code.
