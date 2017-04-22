module Commutativity

lemma : (a:Nat) -> (b:Nat) -> S(a + b) = a + S b
lemma Z b = Refl 
lemma (S k) b = let ih = lemma k b in 
		rewrite ih in Refl	
{--
comm : (a:Nat) -> (b:Nat) -> a + b = b + a
comm Z b = ?comm_aZ
comm (S k) b = let ih = comm k b in 
		?comm_aSk
--}


