open Hw1


(* --------------------- Free vars & free to subst ------------------------------ *)

module StringSet = Set.Make (String);;

(* Function returns set with all free (ie not under lambdas) vars in l *)
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


let reduce_to_normal_form l = failwith "not implemented yet";;