module Commutativity

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

cong2 : (a:Nat) -> (b:Nat) -> (pf: a + b = c) -> (a + (S b) = (S (c)))
cong2 Z b Refl = Refl
cong2 (S k) b Refl = 
	let ih = cong2 k b Refl in
	cong ih	

lemmaNau : (x:Nat) -> (pf : x + x = x + x) -> (S x) + (S x) = S (S x + x)
lemmaNau Z Refl = Refl
lemmaNau (S kk) Refl = 
		let ih = lemmaNau kk Refl in
		let c1 = cong ih in
		let c2 = cong2 (S (S kk)) (S kk) c1 in	
		c2

data Even : Nat -> Type where
	ZE : Even Z
	SSE : Even k -> Even (S (S k))

evenIsEven: Even k -> (x : Nat ** (plus x x) = k) -- ie return that exist such x depending on k
evenIsEven ZE = (Z ** Refl) 
evenIsEven (SSE e) = 
		let (x ** eq) = evenIsEven e in
		let congapp = cong eq in 
		let commres = comm (S x) x in
		?hole










