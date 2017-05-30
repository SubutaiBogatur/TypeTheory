
open Hw1

(* ------------------- Working with primitive lambda calculus --------------------- *)

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

type eq = simp_type * simp_type

type system = eq list

module StringMap = Map.Make (String);; 

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
		match l with 
			Var v -> 
				([], StringMap.find v (if not (StringMap.mem v types) then (StringMap.add v (S_Elem (next_name_generator ())) types) else types)) 
			| App(x, y) -> 
				let sl, tl = impl x types in
				let sr, tr = impl y types in
				let new_type = S_Elem(next_name_generator ()) in
				(List.append sl (List.append sr [(tl, S_Arrow (tr, new_type))]), new_type) 
			| Abs(v, x) -> 
				StringMap.add v S_Elem(next_name_generator ()) types;
				let sr, tr = impl x types in
				(sr, S_Arrow (StringMap.find v types, tr)) in
				 

	None;;
