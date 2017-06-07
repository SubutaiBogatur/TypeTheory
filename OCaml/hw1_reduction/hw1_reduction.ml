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

module StringMap = Map.Make(String)

let ps s = 
	print_string s;
	print_string "\n";;

(* Function changes names of all arguments, that are also free vars. 
	As a result, lambda is converted to alpha-equivalent one,
	all argument names, that coincide with free vars are changed to 
	fresh ones *)
let rename_arguments lm =
	let counter = ref 0 in
	let get_fresh_name () =
		counter := !counter + 1;
		"ϑ" ^ (string_of_int !counter) in 

	(* Function traverses the tree and changes all arguments, whose name
		coincide with free_vars to new ones *)
	let rec hepler lm map =
		match lm with
			Var v -> if (StringMap.mem v map) then (Var (StringMap.find v map)) else lm
			| Abs(v, l) ->  
				(let nn = get_fresh_name () in
				(Abs(nn, hepler l (StringMap.add v nn map)))) 
			| App(lr, ll) -> App(hepler lr map, hepler ll map) in

	hepler lm StringMap.empty;;
	

(* This function is needed to assist in beta reduction. It makes
	one step in normal order. lm is lambda to make step in. *)
let normal_beta_reduction_helper lm =
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
			| Abs(x, r) -> 
				let flag, l_new = impl r in
				(flag, Abs(x, l_new)) in

	let has_happened, new_lm = impl lm in
	new_lm;;

let normal_beta_reduction lm =
	normal_beta_reduction_helper (rename_arguments lm);;

let reduce_to_normal_form l = 
	let rec impl l =
		print_string (string_of_bool (is_normal_form l));
		print_string "\n";
		print_string (Hw1.string_of_lambda l);
		print_string "\n\n";
		if is_normal_form l
			then l
			else impl (normal_beta_reduction l) in
	impl (rename_arguments l);;

let t0 = "(\\f.\\x.f x) x a";; (* sample, which was fixed *)
let k = "(\\x.\\y.x)";;
let i = "(\\x.x)";;
let s = "(\\x.\\y.\\z.x z (y z))";;
let w = "(\\x.x x)";;
let omega = "(" ^ w ^ " " ^ w ^ ")";;
let t1 = k ^ " a " ^ omega;;
let t2 = "(\\x.x) x x";;
let t3 = "(\\f.\\x.f x) (\\f.\\x.x)";;
let t4 = "(\\x.((\\f.(\\x.x)) x))";;

print_string (Hw1.string_of_lambda (reduce_to_normal_form (Hw1.lambda_of_string t4)));;

