

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


(* string of hindley milner type *)
let rec string_of_hmt hmt = match hmt with
                        HM_Elem v -> v
                        | HM_Arrow(hmt1, hmt2) -> (string_of_hmt hmt1) ^ " -> " ^ (string_of_hmt hmt2) 
                        | HM_ForAll(v, hmt) -> "∀" ^ v ^ "." ^ (string_of_hmt hmt);;

(* string of hindley milner lambda ie term *)
let string_of_hml hml = failwith "todo";;

let hml_of_string str = failwith "todo";;

(* print context *)
let pcxt m = StringMap.iter (fun k v -> (print_string ("{" ^ k ^ " " ^ (string_of_hmt v) ^ "}\n"))) m;;

(* print set of strings *)
let print_set s = StringSet.iter (fun s -> (print_string (s ^ "\n"))) s;;




(*
let Some res = Hw2_unify.solve_system [hta t2, hta t3];;
*)





let merge_subst s2 s1 = StringMap.fold (fun k v map -> if StringMap.mem k map then map else StringMap.add k v map) s2 
        (StringMap.fold (fun k v map -> StringMap.add k (ms s2 v) map) s1 StringMap.empty);; 

(* returns type with no quantifiers *)
let rec dwrp t = match t with
        |HM_ForAll(v, lhs) -> ( ms (StringMap.singleton v (HM_Elem(name_generator ()))) (dwrp lhs))
        |_ -> t;;

(*subst to context*)
let stc subst ctxt = StringMap.fold (fun k v map -> (StringMap.add k (ms subst v) map)) ctxt StringMap.empty;;

let rec wepler ctx l = match l with
        |HM_Var v -> (StringMap.empty, dwrp (StringMap.find v ctx))
        |HM_App (x, y) -> 
                        (let s1, t1 = wepler ctx x in
                        let s2, t2 = wepler (stc s1 ctx) y in
                        let fresh = name_generator () in
                        let res = Hw2_unify.solve_system [hta (ms s2 t1), hta
                        (HM_Arrow(t2, HM_Elem (fresh)))] in
                        match res with 
                                |None -> failwith "Robinson fault is not an error" 
                                |Some r -> (
                                        let rob_subst = sath r in
                                        let merged = merge_subst rob_subst (merge_subst s2 s1) in
                                        (merged, ms merged (HM_Elem fresh))))
        |HM_Abs (x, y) -> 
                        (let fresh = name_generator () in
                        let stmp = StringMap.remove x ctx in
                        let stmp = StringMap.add x (HM_Elem(fresh)) stmp in
                        let s1, t1 = wepler stmp y in
                        (s1, HM_Arrow((ms s1 (HM_Elem(fresh))), t1)))
        |HM_Let (x, h1, h2) ->
                        (let s1, t1 = wepler ctx h1 in
                        let sctx = stc s1 ctx in
                        let nctx = StringMap.remove x sctx in
                        let nctx = StringMap.add x (closure t1 sctx) nctx in
                        let s2, t2 = wepler nctx h2 in
                        (merge_subst s2 s1, t2));;

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

let algorithm_w l = 
	
	(* function gets hm_type and returns set with all the free
		type variables in it (ie at least one occurence
		not under quantifier) *)
	let free_vars_hmt hmt = 
		let rec impl hmt blocked = match hmt with
			HM_Elem v -> if StringSet.mem v blocked 
					then StringSet.empty
					else StringSet.singleton v
			| HM_Arrow (hmt1, hmt2) -> StringSet.union (impl hmt1 blocked) (impl hmt2 blocked)        
			| HM_ForAll(v, hmt1) -> impl hmt1 (StringSet.add v blocked) in
		impl hmt StringSet.empty in

	(* Function returns set with all free !type! vars
		in given context *)	
	let free_vars_cxt cxt = StringMap.fold (fun k v set -> StringSet.union (free_vars_hmt v) set) cxt StringSet.empty in

	(* Closure is an operation made on hmt. It returns new hmt.
		What is done: ForAll quantifiers are added to
		given hmt for all free variables, that are met
		in given hmt and also not met in context ctx *)
	let closure hmt ctx = 
		let fvctx = free_vars_context ctx in
		let fvhmt = free_vars_hmt hmt in
		StringSet.fold (fun k t -> HM_ForAll(k, t)) 
			(StringSet.fold 
				(fun k set -> if StringSet.mem k fvctx then set else StringSet.add k set) fvhmt StringSet.empty) hmt in

	(* Function applies substitution subst to type hmt *)
	let apply_substitution subst hmt =
		let rec impl hmt blocked = match hmt with 
				HM_Elem v -> if StringSet.mem v blocked then hmt 
						else if StringMap.mem v subst 
								then StringMap.find v subst 
								else hmt 
				| HM_Arrow (hmt1, hmt2) -> HM_Arrow(impl hmt1 blocked, impl hmt2 blocked)
				| HM_ForAll (v, hmt1) -> HM_ForAll(v, impl hmt1 (StringSet.add v blocked)) in 
		impl hmt StringSet.empty in

	let rec hmt_to_at hmt = match hmt with
			|HM_Elem v -> Hw2_unify.Var v
			|HM_Arrow(hmt1, hmt2) -> Hw2_unify.Fun ("impl", [hta hmt1; hta hmt2])
			|_ -> failwith ("never happens, 'cause according to Artem quantifiers can't be met here");;

	(* no documentation here *)
	let sath sat = let rec ath a = match a with 
				|Hw2_unify.Var v -> HM_Elem v
				|Hw2_unify.Fun ("impl", [a; b]) -> HM_Arrow (ath a, ath b) 
				|_ -> failwith "no warnings pls" in
		List.fold_left (fun map (v, t) -> StringMap.add v (ath t) map) StringMap.empty sat;;

        let s, t = wepler StringMap.empty l in
        Some ((StringMap.bindings s), t);;





















