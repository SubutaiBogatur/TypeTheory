

(* ------------------- Working with primitive lambda calculus --------------------- *)

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

type eq = simp_type * simp_type

type system = eq list

(* --------------- IO for debug ---------------------- *)

module StringMap = Map.Make (String);; 

let p s = (print_string (s ^ "\n"));;

let pm m =
	let rec impl l =
		match l with
			|[] -> print_string "\n"
			|((k, (S_Elem e))::t) -> print_string (k ^ " "); p e; impl t 
			|_ -> () in

	impl (StringMap.bindings m);; 
	



(* Function gets lambda and infers type for it. It also returns
	types of variables, that are met in lambda (ie context) *)
(* lambda -> ((string * simp_type list) * simp_type) option *)
let infer_simp_type l =
	let new_name_counter = ref 0 in

	(* Function generates next fresh name to use as type variable *)     
        (* unit -> string *)
        let next_name_generator () =
                new_name_counter:= !new_name_counter + 1; 
                "ß" ^ string_of_int !new_name_counter in

	(* Recursice function that on each step returns tuple: system and type 
		It gets map of types (every variable is mapped to its type *)
	(* lambda -> StringMap -> (system * simp_type) *)
	let rec impl l types =
		print_string ((Hw1.string_of_lambda l) ^ "\n");
		pm types;
		match l with 
			Hw1.Var v -> 
				([], StringMap.find v types) 
			| Hw1.App(x, y) -> 
				let sl, tl = impl x types in
				let sr, tr = impl y types in
				let new_type = S_Elem(next_name_generator ()) in
				(List.append sl (List.append sr [(tl, S_Arrow (tr, new_type))]), new_type) 
			| Hw1.Abs(v, x) -> 
				let upd_type = StringMap.add v (S_Elem(next_name_generator ())) types in
				let sr, tr = impl x upd_type in
				(sr, S_Arrow (StringMap.find v upd_type, tr)) in
		
	(* Function gets list of free variables in lambda and gives a unique
		type to every one *)
	(* string list -> StrinMap -> StringMap *)		 
	let rec list_to_map l m =
		match l with
			[] -> m
			| h::t -> list_to_map t (StringMap.add h (S_Elem(next_name_generator ())) m) in

	(* Functions below convert different structs of simple types to 
		algebraic types. Every implication in simple type is
		considered a function with name impl and two arguments *)
	let rec st_to_at st =
		match st with 
			S_Elem v -> Hw2_unify.Var v 
			| S_Arrow (stl, str) -> Hw2_unify.Fun ("impl", [st_to_at stl; st_to_at str]) in   

	let st_to_at_sys sys = 
		let rec st_to_at_eq eq =
			let l, r = eq in
			(st_to_at l, st_to_at r) in

		let rec impl sold snew =
			match sold with 
				[] -> snew
				| h::t -> impl t ((st_to_at_eq h)::snew) in
		impl sys [] in

	let rec at_to_st at =
		match at with 
			Hw2_unify.Var v -> S_Elem v
			| Hw2_unify.Fun (name, [lhs; rhs]) -> S_Arrow (at_to_st lhs, at_to_st rhs) 
			| _ -> failwith "Never happens, no panic guys" in

	(* Function converts solution, which was received from Robinson 
		(ie list of pairs (string, algebraic_term) to solution
		in the terms of types ie (string, simple_type) *)
	let at_to_st_solution solution =
		let rec impl at_sol simple_sol =
			match at_sol with
				[] -> simple_sol
				| ((var, at)::t) -> impl t ((var, at_to_st at)::simple_sol) in

		impl solution [] in
		
	let simple_type_sys, res_type = impl l (list_to_map (Hw1_reduction.free_vars l) StringMap.empty) in
	Hw2_unify.psys (st_to_at_sys simple_type_sys);	
	let rr = Hw2_unify.solve_system (st_to_at_sys simple_type_sys) in (* robinson result *)
	match rr with 
		None -> None
		| Some solution -> Some (at_to_st_solution solution, 
				at_to_st(Hw2_unify.apply_substitution solution (st_to_at res_type)));;

(* Hahahahahha, test with Omega Omega combinator: *)
infer_simp_type (Hw1.lambda_of_string "(\\x.x x) (\\x.x x)");;
