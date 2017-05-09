module Commutativity

||| First homework ie proving commutativity of addition
lemmaAbbA : a = b -> b = a
lemmaAbbA Refl = Refl

lemma0 : (a:Nat) -> (b:Nat) -> S(a + b) = a + S b
lemma0 Z b = Refl 
lemma0 (S k) b = let ih = lemma0 k b in 
		rewrite ih in Refl	

lemma1 : (a:Nat) -> (b:Nat) -> a + S b = S(a + b)
lemma1 a b = lemmaAbbA (lemma0 a b)

redZRhs : (a:Nat) -> a = a + Z
redZRhs Z = Refl
redZRhs (S k) = cong (redZRhs k) 

comm : (a:Nat) -> (b:Nat) -> a + b = b + a
comm Z b = redZRhs b 
comm (S k) b = let ih = comm k b in 
		let lem = lemma1 b k in 
		let prev = cong ih in 
		rewrite lem in prev


||| Here second homework starts
data Even : Nat -> Type where
	ZE : Even Z
	SSE : Even k -> Even (S (S k))

getLen : Even k -> Nat
getLen ZE = Z
getLen (SSE e) = (getLen e) + 2

getHalfLen : Even k -> Nat
getHalfLen ZE = Z
getHalfLen (SSE e) = (getHalfLen e) + 1

tmp1 : (a : Nat) -> (b : Nat) -> (k : Nat) -> a + b = k -> (S a) + b = (S k)
tmp1 a b k eq = cong eq

tmp2 : (a : Nat) -> (b : Nat) -> (k : Nat) -> a + b = k -> a + (S b) = (S k)
tmp2 a b k eq =
	rewrite (lemma1 a b) in (cong eq)
	

doubleInc : (x : Nat) -> (k : Nat) -> x + x = k -> (S x) + (S x) = (S (S k))
doubleInc x k eq = 
	tmp1 x (S x) (S k) u where
		u = tmp2 x x k eq


evenIsEven : Even k -> (x : Nat ** x + x = k) -- ie return that exist such x depending on k
evenIsEven ZE = (Z ** Refl) 
evenIsEven (SSE e) =  
		let ih = evenIsEven e in
		?eie









