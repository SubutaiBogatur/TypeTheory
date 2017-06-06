

let debug_enabled = false;;

(* ------------------- Working with primitive lambda calculus --------------------- *)

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

(* --------------- IO for debug ---------------------- *)

module StringMap = Map.Make (String);; 

(* print string *)
let p s = (print_string (s ^ "\n"));;

(* print map of string -> simple type *)
let pst_map m =
	let rec impl l =
		match l with
			|[] -> print_string "\n"
			|((k, (S_Elem e))::t) -> print_string (k ^ " "); p e; impl t 
			|_ -> () in

	impl (StringMap.bindings m);; 

let string_of_simp_type st = 
	let rec impl st str = 
		match st with 
			S_Elem v -> str ^ v
			| S_Arrow (x, y) -> str ^ (impl x "") ^ " -> " ^ (impl y "") in
	impl st "";;
	

(* ------------ Primitive lambda calculus type inference --- *)

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
		List.map (fun (x, y) -> (st_to_at x, st_to_at y)) sys in

	let rec at_to_st at =
		match at with 
			Hw2_unify.Var v -> S_Elem v
			| Hw2_unify.Fun (name, [lhs; rhs]) -> S_Arrow (at_to_st lhs, at_to_st rhs) 
			| _ -> failwith "Never happens, no panic guys" in

	(* Function converts solution, which was received from Robinson 
		(ie list of pairs (string, algebraic_term) to solution
		in the terms of types ie (string, simple_type) *)
	let at_to_st_solution solution =
		List.map (fun (var, at) -> (var, at_to_st at)) solution in
		
	let simple_type_sys, res_type = impl l (list_to_map (Hw1_reduction.free_vars l) StringMap.empty) in
	Hw2_unify.psys (st_to_at_sys simple_type_sys);	
	let rr = Hw2_unify.solve_system (st_to_at_sys simple_type_sys) in (* robinson result *)
	match rr with 
		None -> None
		| Some solution -> Some (at_to_st_solution solution, 
				at_to_st(Hw2_unify.apply_substitution solution (st_to_at res_type)));;

(* Hahahahahha, test with Omega Omega combinator fails, cool *)
(*
let Some (f, s) = infer_simp_type (Hw1.lambda_of_string "\\x.\\y.x");;
print_string (("Resulting type: " ^ (st_to_string s)) ^ "\n");;
*)


(* ---------------------- Hindley Milner and his W algorithm --------------- *)

module StringSet = Set.Make (String) 

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

(* -------------------------- Util functions including IO ------------------ *)

(* print string *)
let ps s = 
	print_string s;
	print_string "\n";;

(* string of hindley milner type *)
let rec string_of_hmt hmt = 
		match hmt with
                        HM_Elem v -> v
                        | HM_Arrow(hmt1, hmt2) -> "(" ^ (string_of_hmt hmt1) ^ " -> " ^ (string_of_hmt hmt2) ^ ")" 
                        | HM_ForAll(v, hmt) -> "∀" ^ v ^ "." ^ (string_of_hmt hmt);;

(* string of hindley milner lambda ie term *)
let rec string_of_hml hml =
	match hml with 
		HM_Var v -> v
		| HM_Abs(v, hml) -> ("\\" ^ v ^ "." ^ "(" ^ (string_of_hml hml) ^ ")")
		| HM_App(hml1, hml2) -> ("(" ^ (string_of_hml hml1) ^ " " ^ (string_of_hml hml2) ^ ")")
		| HM_Let(v, hml1, hml2) -> ("let " ^ v ^ " = (" ^ (string_of_hml hml1) ^ ") in (" ^ (string_of_hml hml2)) ^ ")";;


(* print context *)
let pcxt m = StringMap.iter (fun k v -> (print_string ("{" ^ k ^ " " ^ (string_of_hmt v) ^ "}\n"))) m;;

(* print set of strings *)
let print_set s = StringSet.iter (fun s -> (print_string (s ^ "\n"))) s;;


exception InferenceException of string;; 

(* Algorithm W infers a type for a term in Hindley
	Milner type system. Basic concepts used in the code:
	- Context is a map from string to hmt (var with its type)
	- Substitution is a map from string to hmt (it is made on types
		obviously)
	
	Commonly used abbreviations:
	- hmt is Hindley Milner type
	- hml is Hindley Milner lambda ie term
	- cxt is context
	- at is algebraic term (see Robinson algorithm for usages)

	Subsequent code uses folds on sets, maps and lists a lot and
		some of them are rather complex, consider yourself warned *)

let algorithm_w hml = 

	let new_name_counter = ref 0 in

	(* Function generates next fresh name to use as type variable *)     
        (* unit -> string *)
        let next_name_generator () =
                new_name_counter:= !new_name_counter + 1; 
                "α" ^ string_of_int !new_name_counter in

        let initial_name_generator () =
                new_name_counter:= !new_name_counter + 1; 
                "ζ" ^ string_of_int !new_name_counter in
	
	(* function gets hm_type and returns set with all the free
		type variables in it (ie at least one occurence
		not under quantifier) *)
	let free_vars_hmt hmt = 
		let rec impl hmt blocked = 
			match hmt with
				HM_Elem v -> if StringSet.mem v blocked 
						then StringSet.empty
						else StringSet.singleton v
				| HM_Arrow (hmt1, hmt2) -> StringSet.union (impl hmt1 blocked) (impl hmt2 blocked)        
				| HM_ForAll(v, hmt1) -> impl hmt1 (StringSet.add v blocked) in
		impl hmt StringSet.empty in


	(* Function returns set with all free !type! vars
		in given context *)	
	let free_vars_cxt cxt = StringMap.fold (fun k v set -> StringSet.union (free_vars_hmt v) set) cxt StringSet.empty in

	let free_vars_hml hml =
		let rec impl hml blocked =
			match hml with
				HM_Var v -> if StringSet.mem v blocked then StringSet.empty else StringSet.singleton v
				| HM_Abs(v, hml) -> impl hml (StringSet.add v blocked)
				| HM_Let(v, hml1, hml2) -> StringSet.union (impl hml1 blocked) (impl hml2 (StringSet.add v blocked))
				| HM_App(hml1, hml2) -> StringSet.union (impl hml1 blocked) (impl hml2 blocked) in 
		impl hml StringSet.empty in

	(* Closure is an operation made on hmt. It returns new hmt.
		What is done: ForAll quantifiers are added to
		given hmt for all free variables, that are met
		in given hmt and also not met in context ctx *)
	let closure hmt ctx = 
		let fvctx = free_vars_cxt ctx in
		let fvhmt = free_vars_hmt hmt in
		StringSet.fold (fun k t -> HM_ForAll(k, t)) 
			(StringSet.fold 
				(fun k set -> if StringSet.mem k fvctx then set else StringSet.add k set) fvhmt StringSet.empty) hmt in

	(* Function applies substitution subst to type hmt *)
	let apply_subst subst hmt =
		let rec impl hmt blocked = 
			match hmt with 
				HM_Elem v -> if StringSet.mem v blocked then hmt 
						else if StringMap.mem v subst 
								then StringMap.find v subst 
								else hmt 
				| HM_Arrow (hmt1, hmt2) -> HM_Arrow(impl hmt1 blocked, impl hmt2 blocked)
				| HM_ForAll (v, hmt1) -> HM_ForAll(v, impl hmt1 (StringSet.add v blocked)) in 
		impl hmt StringSet.empty in

	(* Apply substitution to context *)
	let apply_subst_cxt subst cxt = StringMap.fold (fun k v map -> (StringMap.add k (apply_subst subst v) map)) cxt StringMap.empty in

	(* Convert hmt to at to use it later in Robinson unification *)
	let rec hmt_to_at hmt = 
		match hmt with
			HM_Elem v -> Hw2_unify.Var v
			| HM_Arrow(hmt1, hmt2) -> Hw2_unify.Fun ("impl", [hmt_to_at hmt1; hmt_to_at hmt2])
			| _ -> failwith "Never happens, 'cause according to Artem, quantifiers can't be met here" in 

	(* Convert substritution in algebraic terms ie list (returned by Robinson)
		to substitution in hmt ie map *)
	let subst_at_to_subst_hmt subst_at = 
		let rec at_to_hmt at = 
			match at with 
				Hw2_unify.Var v -> HM_Elem v
				| Hw2_unify.Fun ("impl", [a; b]) -> HM_Arrow (at_to_hmt a, at_to_hmt b) 
				| _ -> failwith "no warnings pls" in
		List.fold_left (fun map (var, at) -> StringMap.add var (at_to_hmt at) map) StringMap.empty subst_at in

	(* Function gets two substitutions and merges them. 
		Firstly s1 is applied, then s2 is applied. So,
		s2 is applied to s1 in implementation and then merge is made 
		eg s1 = {x -> u, y -> v}, s2 = {u -> a, x -> b, z -> f} then result is
			{x -> a, y -> v, z -> f}  *)
	let merge_subst s2 s1 = 
		StringMap.fold (fun k v map -> if StringMap.mem k map then map else StringMap.add k v map) s2 
			(StringMap.fold (fun k v map -> StringMap.add k (apply_subst s2 v) map) s1 StringMap.empty) in

	(* Function is used in the first case of W algo (when hml is var)
		All the quantifiers (they are only on the surface) are
		removed from the type and instead of that quantifiers
		substitution of fresh vars is made *)
	let rec dewrap hmt = 
		match hmt with
			HM_ForAll(v, hmt1) -> (apply_subst (StringMap.singleton v (HM_Elem(next_name_generator ()))) (dewrap hmt1))
			| _ -> hmt in


	(* Algorithm by itself is here. It was written to algorithm
		scheme described for example in Artem's Ohanjanyan conspect (see github) *)
	let rec impl ctx hml = 
		match hml with
			HM_Var v -> 
				if not (StringMap.mem v ctx) then raise (InferenceException "Free variable encountered")
				else (StringMap.empty, dewrap (StringMap.find v ctx))
			| HM_App (x, y) -> 
					(let s1, t1 = impl ctx x in
					let s2, t2 = impl (apply_subst_cxt s1 ctx) y in
					let fresh = next_name_generator () in
					let res = Hw2_unify.solve_system [hmt_to_at (apply_subst s2 t1), hmt_to_at (HM_Arrow(t2, HM_Elem (fresh)))] in
					match res with 
						None -> raise (InferenceException "Robinson has failed to unificate types") 
						| Some r -> (
							let rob_subst = subst_at_to_subst_hmt r in
							let merged = merge_subst rob_subst (merge_subst s2 s1) in
							(merged, apply_subst merged (HM_Elem fresh))))
			| HM_Abs (x, y) -> 
					(let fresh = next_name_generator () in
					let stmp = StringMap.remove x ctx in
					let stmp = StringMap.add x (HM_Elem(fresh)) stmp in
					let s1, t1 = impl stmp y in
					(s1, HM_Arrow((apply_subst s1 (HM_Elem(fresh))), t1)))
			| HM_Let (x, h1, h2) ->
					(let s1, t1 = impl ctx h1 in
					let sctx = apply_subst_cxt s1 ctx in
					let nctx = StringMap.remove x sctx in
					let nctx = StringMap.add x (closure t1 sctx) nctx in
					let s2, t2 = impl nctx h2 in
					(merge_subst s2 s1, t2)) in

	let free_set = free_vars_hml hml in
	let initial_context = StringSet.fold (fun var map -> StringMap.add var (HM_Elem(initial_name_generator ())) map) free_set StringMap.empty in
	try
	 	(* todo add free vars search for hml *)	
        	let set, hmt = impl initial_context hml in
        	Some ((StringMap.bindings set), hmt)
	with (InferenceException msg) ->
		if debug_enabled then ps msg else print_string "";
		None;;



