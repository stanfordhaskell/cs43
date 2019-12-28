---
title: Algebraic Datatypes
---

## Motivating examples

We have seen how every value in Haskell must have a type, and that values can
only be combined according to their types. While Haskell has many built-in
types, these are not sufficient for representing all the "types" of values we
might want to use. For this reason, Haskell allows us to define new types and
use these just as we might use any of the built-in types. Let us consider three
cases in which using only the built-in types limits our expressive power, and
see how Haskell's powerful type system allows us to write clear and safe code.

### Composite values

A common design pattern in object-oriented languages involves representing a
record with fields corresponding to attributes. For example, in Python we might
represent a student as a `Student` object.

```python
class Student:
    def __init__(self, name, age, gpa):
        self.name = name
        self.age = age
        self.gpa = gpa
```

Since Haskell only represents values with types, to do the same in Haskell we
need to find a value with a type capable of representing a student's attributes. 
First, note that each attribute has a natural built-in type. Age is
representable as an `Int`, GPA as a `Float`, and name as a
`String`. Note that the `String` type in Haskell is a synonym for the type of a list of characters, `[Char]`. Lists will be
discussed at the bottom of these notes, but for now it is sufficient that they
are of arbitrary length and contain values of a single type.

```haskell
ghci> ("Maya" :: [Char]) == ("Maya :: String)
True

ghci> :info String
type String = [Char]   -- definition of a type synonym
```

The values above would not even be comparable in Haskell if they were of
different types, since `(==)` accepts values of the same type (so long as that
type is in the `Eq` typeclass and therefore comparable for equality).

```haskell
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool -- two inputs of the same type a

ghci> (1 :: Int) == (1 :: Float)
```
```error
error:
    • Couldn't match expected type ‘Int’
      with actual type ‘Float’
```

While the fields themselves have natural built-in types, the problem here is
that we need a type which can contain `Int` and `String` values. One solution is
to use Haskell's built-in tuple types.

A tuple type `(T1, T2, T3, ..., Tn)` represents a value containing values of
types `T1` through `Tn`. Unlike lists, they can contain values of different
types, but only as they appear in the type signature. Thus, the types `(Bool,
Bool)` and `(Bool, Bool, Bool)` are different types, as are `(Bool, Int)` and
`(Int, Bool)`.

```haskell
ghci> :t (True, False)
... :: (Bool, Bool)

ghci> :t (True, False, 1 :: Int)
... :: (Bool, Bool, Int)

ghci> :t (not, incInt)
... :: (Bool -> Bool, Int -> Int)
```

Just like number constants and functions, tuples can be polymorphic in the types
they contain, and specialize to different concrete types depending on context.

```haskell
ghci> :t (1, 1.5, inc)
... :: (Fractional b, Num a1, Num a2) => (a1, b, a2 -> a2)
```

Tuples allow us to combine the values needed to represent a student, for example
using the type `(String, Int, Float)`. 

```haskell
ghci> :t ("Ron", 16, 2.4) :: (String, Int, Float)
... :: (String, Int, Float)
```

However, this is neither semantically expressive nor does it provide any type
safety. To demonstrate this, consider a function that returns a student's GPA.

```haskell
gpa (_, _, gpa) = gpa  -- "pattern matches" on input
```

When Haskell infers the type of this function, all it requires is that the input
is a tuple of length three in order to satisfy the pattern matching.

```haskell
ghci> :t gpa
gpa :: (a, b, c) -> c
```

This tells us nothing about what type of value the function expects, and lets us
pass it any tuple of length three. We could increase the safety of the function by
specifying a type signature with the definition.

```haskell
gpa :: (String, Int, Float) -> Float
gpa (_, _, gpa) = gpa
```

While this guarentees that the input and output will be of the desired type, it
does not make the type signature expressive of the function's meaning. This
could be remedied by making synonyms for our types and using those in our type
signature.

```haskell
type Name = String
type Age = Int
type GPA = Float
type StudentTuple = (Name, Age, GPA)

gpa :: StudentTuple -> Float
gpa (_, _, gpa) = gpa
```

While this appears to solve the problem, `StudentTuple` is merely a synonym
for `(String, Int, Float)` and Haskell will not prevent us from passing any such
tuple to `gpa` even when this makes no sense semantically.

```haskell
ghci> gpa ("Silicon", 14, 28.085)
28.085
```

What we really want to do is create a new `Student` type, so that we can use
values of type `Student` instead of values of type `(String, Int, Float)`. The
`data` keyword allows us to do exactly that. The following code creates a *new* type
called `Student`.

```haskell
data Student = Student String Int Float
    deriving (Show) -- lets Haskell decide how to print the type
```

The syntax here is

```haskell
data TypeName = TypeConstructor T1 T2 ...
```

and it is customary to give the same name to the type and its constructor.^[In a
moment we will see that types can have multiple constructors, in which case this
is no longer done.]

We can create values of type `Student` by using the datatype's constructor,

```haskell
ghci> :t Student "Hermione" 16 2.4
Student "Hermione" 16 2.4
```

and we can pattern match on the constructor to write functions that accept the
type.

```haskell
gpa (Student _ _ gpa) = gpa
```

Note that here Haskell can infer the correct input type for the function from
the pattern matching.

```haskell
ghci> gpa (Student "Hermione" 16 4.3)
4.3

ghci> :t gpa
gpa :: Student -> Float
```

### Optional values

Coming.

### Alternate values

Coming.
