module Commutativity

lemma : (a:Nat) -> (b:Nat) -> S(a + b) = a + S b
lemma Z b = Refl 
lemma (S k) b = let ih = lemma k b in 
		rewrite ih in Refl	

lemma1 : (a:Nat) -> (b:Nat) -> a + S b = S(a + b)
lemma1 Z b = ?lemma1_Z
lemma1 (S k) b = ?lemma1_Ska

redZRhs : (a:Nat) -> a = a + Z
redZRhs Z = Refl
redZRhs (S k) = cong (redZRhs k) 

comm : (a:Nat) -> (b:Nat) -> a + b = b + a
comm Z b = redZRhs b 
comm (S k) b = let ih = comm k b in 
		let lem = lemma1 b k in -- b + S k = S(b + k)
		let prev = cong ih in -- S(k + b) = S(b + k)
		--rewrite lem in prev  
		rewrite lem in prev
		




