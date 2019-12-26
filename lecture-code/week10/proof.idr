module Proof

{- Equality type
data (=) : a -> b -> Type where
   Refl : x = x
-}

four_eq_four : 4 = 4
four_eq_four = Refl

{-
four_eq_five : 4 = 5
four_eq_five = Refl
-}

-- Unification and normal form

{- 
plus : Nat -> Nat -> Nat
plus Z     m = m
plus (S k) m = S (plus k m)
-}

twoplustwo_eq_four : 2 + 2 = 4
twoplustwo_eq_four = Refl

plus_reduces_Z_right : (m : Nat) -> plus Z m = m
plus_reduces_Z_right m = Refl

-- > :t plus_reduces_Z_right 
-- > plus_reduces_Z_right 4


plus_reduces_Z_left : (m : Nat) -> plus m Z = m
plus_reduces_Z_left m = Refl

{- successful proof
plus_reduces_Z_left' : (n:Nat) -> n = plus n Z
plus_reduces_Z_left' Z = Refl
plus_reduces_Z_left' (S k) = cong (plus_reduces_Z_left' k)
-}

-- TODO half-derive above


-- plus_commutes : (n : Nat) -> (m : Nat) -> n + m = m + n

-- TODO half-derive above

-- > :t plusCommutative
-- > plusCommutative 4 5 
