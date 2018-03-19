module Test where

data ℕ : Set where
  Z : ℕ
  S :  ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

data Fin : ℕ → Set where
  fzero : {n : ℕ} → Fin (S n)
  fsucc : {n : ℕ} → Fin n → Fin (S n)
{--
foo : Fin Z → ℕ
foo ()

bar : Fin (S Z) → ℕ
bar fzero = Z
bar (fsucc ())

buz : Fin (S (S Z)) → ℕ
buz fzero = Z
buz (fsucc fzero) = S Z
buz (fsucc (fsucc ()))
--}

data Vec (A : Set) : (n : ℕ) → Set where
  []  : Vec A Z
  _∷_ : ∀ {n} → A → Vec A n → Vec A (S n)

infixr 4 _∷_

{--
foo : Vec ℕ Z → ℕ
foo [] = Z

bar : Vec ℕ (S Z) → ℕ
bar [] = Z
bar (x ∷ xs) = x
--}

indexAt : {A : Set}{n : ℕ} → Vec A n → Fin n → A
indexAt (x ∷ xs) fzero = x
indexAt (x ∷ xs) (fsucc i) = indexAt xs i

_+_ : ℕ → ℕ → ℕ
Z + y = y
S x + y = S (x + y)

_*_ : ℕ → ℕ → ℕ
Z * y = Z
S x * y = y + (x * y)

-- inner product
_⊗_ : ∀ {n} → Vec ℕ n → Vec ℕ n → ℕ
[] ⊗ ys = Z
(x ∷ xs) ⊗ (y ∷ ys) = (x * y) + (xs ⊗ ys)

ex1 : ℕ
ex1  = (9 ∷ 3 ∷ 7 ∷ 8 ∷ 1 ∷ 2 ∷ 6 ∷ []) ⊗ (1 ∷ 3 ∷ 2 ∷ 4 ∷ 9 ∷ 0 ∷ 8 ∷ [])
