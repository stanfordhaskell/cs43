module Hello

{- Examples of algebraic datatypes

data List a = Nil | (::) a (List a)

data Nat = Z | S Nat 

-}

-- > :t Nil
-- > :t (::) 

-- > :t Z
-- > :t S
-- > :t S Z
-- > :t S (S Z)

-- > :t Nat
-- > :t List

{- Example 0: Type aliases -}

Words : Type
Words = String

{- Example 1: Calculating types -}

{- Example 2: Printf -}

%default total

-- Formatting AST.
data Format
  = FInt Format
  | FString Format
  | FOther Char Format
  | FEnd

format : List Char -> Format
format ('%' :: 'd' :: cs ) = FInt ( format cs )
format ('%' :: 's' :: cs ) = FString ( format cs )
format ( c :: cs )         = FOther c ( format cs )
format []                  = FEnd

formatString : String -> Format
formatString s = format ( unpack s )

-- > formatString "%s,%d"

interpFormat : Format -> Type
interpFormat (FInt f)     = Int -> interpFormat f
interpFormat (FString f)  = String -> interpFormat f
interpFormat (FOther _ f) = interpFormat f
interpFormat FEnd         = String

toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction ( FInt f ) a     = \i => toFunction f ( a ++ show i )
toFunction ( FString f ) a  = \s => toFunction f ( a ++ s )
toFunction ( FOther c f ) a = toFunction f ( a ++ singleton c )
toFunction FEnd a           = a

-- > :t toFunction (formatString "%s,%d")

printf : (s : String) -> interpFormat ( formatString s )
printf s = toFunction ( formatString s ) ""

-- > :t printf
-- > :t printf "%s,%d"


{- Example 3: Vectors

data List a = Nil | (::) a (List a)

data Nat = Z | S Nat 

-}

data Vect : Nat -> Type -> Type where
   Nil  : Vect Z a
   (::) : a -> Vect k a -> Vect (S k) a


(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) [] ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

-- TODO show derivation
-- TODO show error


zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith f [] [] = []
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys


-- TODO show derivation


data Fin : Nat -> Type where
   FZ : Fin (S k)
   FS : Fin k -> Fin (S k)

index : Fin n -> Vect n a -> a
index FZ (x :: xs) = x
index (FS x) (y :: ys) = index x ys

-- TODO show derivation and error in derivation


-- isEmpty : Vect n a -> Bool

-- TODO show case split two versions
