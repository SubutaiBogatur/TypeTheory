open Hw1


(* --------------------- Free vars & free to subst ------------------------------ *)

module StringSet = Set.Make (String);;

(* Function returns set with all free (ie at least one occurencenot under lambda)
	 vars in l *)
(* lambda -> StringSet -> StringSet*)	
let rec all_free_vars l blocked =
	match l with
		Var v ->
			if StringSet.mem v blocked
				then StringSet.empty
				else StringSet.singleton v
		| Abs(v, x) ->
			all_free_vars x (StringSet.add v blocked)
		| App(x, y) ->
			StringSet.union (all_free_vars x blocked) (all_free_vars y blocked);; 

(* Same as all_free_vars, but returns list, not set *)
(* lambda -> string list *)
let free_vars lm =
	let set = all_free_vars lm StringSet.empty in
	StringSet.elements set;; 

(* Functions checks if theta is free for substitution instead of var in alpha *)
(* lambda -> lambda -> string -> bool *)
let free_to_subst theta alpha var =

	(* Function gets theta to substitute in l instead of all occurences of 
		v in l. Moreover it gets two sets: set of all free vars in theta
		and set of vars blocked by lambda in current node. Function
		returns bool: true if no free vars from theta become blocked
		when theta is substituted instead of all !free! occurences
		of v in l*)
	(* StringSet -> StringSet -> lambda -> lambda -> string -> bool*) 
	let rec impl fs bs l v =
		match l with
			App(x, y) -> (impl fs bs x v) && (impl fs bs y v)
			| Abs(u, x) -> if u = v then true
						else impl fs (StringSet.add u bs) x v
			| Var(u) -> if u = v then ((StringSet.inter fs bs) = StringSet.empty)
						else true in

	let free_set = all_free_vars theta StringSet.empty in
	impl free_set StringSet.empty alpha var;;


(* ---------------------------------- Normal form ------------------------------- *)

let is_normal_form lm =
	
	(* Function recursively searches for beta redex in given lambda
		and returns false if found, else true *)
	(* lambda -> bool *)
	let rec no_beta_redex lm =
		match lm with
			Var x -> true
			| App(Abs(x, rinner), r) -> false (* it is typical beta redex, game over *)
			| App(l, r) -> (no_beta_redex l) && (no_beta_redex r) 		
			| Abs(x, r) -> no_beta_redex r in

	no_beta_redex lm;;


(* Function substitutes term theta in all !free! occurences of v_old in lambda *)
(* lambda -> string -> lambda -> lambda *)
let rec subst lm v_old theta =
	match lm with 
		Var v -> if v = v_old then theta else lm
		| Abs (v, x) -> 
			(* If v_old is enslaved by lambda, no more subst *)
			if v = v_old then lm else Abs (v, subst x v_old theta)
		| App (x, y) ->
			App (subst x v_old theta, subst y v_old theta);; 

(* lambda -> lambda -> bool *)
let is_alpha_equivalent x y =
	let subst_counter = ref 0 in
	
	(* Function generates next name for substitution *)	
	(* unit -> string *)
	let next_name_generator () =
		let res = "t" ^ string_of_int !subst_counter in
		subst_counter := !subst_counter + 1; 
		res in

	let rec impl x y =
		match (x, y) with
			(Var a, Var b) -> a = b (* Check if same vars *)
			| (App (p, q), App(r, s)) ->
				(impl p r) && (impl q s)	
			| (Abs (v1, p), Abs (v2, q)) ->
				let v = (next_name_generator ()) in
				impl (subst p v1 (Var v)) (subst q v2 (Var v))
			| _ -> false in
	impl x y;; 

let normal_beta_reduction lm =
	
	(* Function tries to find first beta redex and
		simplify it. It returns pair: (bool, lambda)
		ie if simplification was made and new lambda *)
	let rec impl lm =
		match lm with
			Var x -> (false, Var x)
			| App(Abs(x, ri), ro) -> (true, subst ri x ro)
			| App(l, r) -> 
				let flag, l_new = impl l in
				if flag then 
					(true, App(l_new, r)) else
					let flag, r_new = impl r in
					(flag, App(l_new, r_new)) 
			| Abs(x, r) -> impl r in

	let has_happened, new_lm = impl lm in
	new_lm;;


let reduce_to_normal_form l = 
	let rec impl l =
		print_string (string_of_bool (is_normal_form l));
		print_string "\n";
		print_string (Hw1.string_of_lambda l);
		print_string "\n\n";
		if is_normal_form l
			then l
			else impl (normal_beta_reduction l) in
	impl l;;

(*
print_string (Hw1.string_of_lambda (reduce_to_normal_form (Hw1.lambda_of_string "((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.((\\l11.((\\l12.((\\l13.((\\l14.((\\l15.((\\l16.((\\l17.((\\l18.(l18 (\\l19.(\\l20.(l19 (l19 (l19 (l19 (l19 (l19 (l19 (l19 (l19 l20))))))))))))) (\\l18.(l4 (((l17 l18) (\\l19.(\\l20.l20))) l18))))) (l0 (\\l17.(\\l18.(\\l19.(\\l20.(((l1 ((l9 l19) l20)) l19) ((\\l21.(((l1 ((l16 (l14 l21)) l18)) (((l17 l18) ((l6 l21) (\\l22.(\\l23.(l22 l23))))) l20)) (((l17 l18) l19) l21))) (l15 ((l6 l19) l20))))))))))) (l0 (\\l16.(\\l17.(\\l18.((l10 (l8 l17)) (((l1 (l8 l18)) l3) ((l16 ((l7 l17) (\\l19.(\\l20.(l19 l20))))) ((l7 l18) (\\l19.(\\l20.(l19 l20))))))))))))) (l0 (\\l15.(\\l16.(((l1 (l8 (l4 l16))) (\\l17.(\\l18.l18))) ((l6 (\\l17.(\\l18.(l17 l18)))) (l15 (l4 (l4 l16)))))))))) (\\l14.(\\l15.(l14 (l14 l15)))))) (\\l13.((((l0 (\\l14.(\\l15.(\\l16.(\\l17.(((l1 (l8 l15)) l17) (((l14 (l4 l15)) l17) ((l6 l16) l17)))))))) l13) (\\l14.(\\l15.l15))) (\\l14.(\\l15.(l14 l15))))))) (\\l12.(\\l13.(\\l14.((l14 l12) l13)))))) (l0 (\\l11.(\\l12.(\\l13.(((l1 (l8 l12)) (\\l14.(\\l15.l15))) ((l6 l13) ((l11 (l4 l12)) l13))))))))) (\\l10.(\\l11.(((l1 l10) l2) l11))))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\l13.(((l1 l12) l13) (((l1 l13) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))")));;
*)
(*
print_string (Hw1.string_of_lambda (reduce_to_normal_form (Hw1.lambda_of_string "(\\x.x) y")));;
*)


