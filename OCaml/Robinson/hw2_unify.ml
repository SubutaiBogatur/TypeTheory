type algebraic_term = Var of string | Fun of string * (algebraic_term list)


(* todo beatiful and nice and elegant visualization pls *)
let rec list_to_string x str = match x with
         |[]-> str
         |h::t -> type_to_string h (" " ^  (list_to_string t str))

and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) -> "(" ^ a ^ " " ^ (list_to_string b str) ^ ")" ;;

let rec system_to_string x str = match x with
        |[] -> str
        |(l,r)::t -> type_to_string l "" ^ "= " ^  type_to_string r "" ^ "\n" ^ system_to_string t str;;

let print_equation eq str =
	system_to_string [eq] str;;

let print_eq sys = 
	print_equation sys "";; 


(* ------------------------ System to Equation ------------------------------ *)

(* Function gets system of equation as an input and returns
	function name that is never met in the system. The name
	is generated as concatenation of all names of 
	function. Maybe implementation should be changed *)
let get_fresh_fun_name system =

	(* Function with same meaning but for one algebraic term *)
	let rec get_fresh_name_at at =
		let rec impl at str = 
			match at with
				Var v -> str (* maybe should do v ^ str *)
				| Fun (f, atl) -> f ^ get_fresh_name_l atl in 
		impl at ""
	
	
	and get_fresh_name_l l =
		let rec impl l str =
			match l with
				[] -> str
				| (h :: t) -> (get_fresh_name_at h) ^ (impl t str) in
		impl l "" in

	(* Function gets system and string to concat with *)
	let rec impl system str =
		match system with
			[] -> str
			| (lhs, rhs)::t -> (get_fresh_name_at lhs) ^ (get_fresh_name_at rhs) ^ (impl t str) in 
	impl system "fresh";;
			

let rec eq_helper x ll rr = match x with 
        |[] -> ll, rr
        |(lh, rh)::t -> eq_helper t (lh::ll) (rh::rr);; 

let system_to_equation x = 
        let l, r = eq_helper x [] [] in
        let fresh_name = get_fresh_fun_name x in
	(Fun(fresh_name, l), Fun(fresh_name, r));;


(* Test samples *)

let sys0 = [(Var "a", Var "b"); (Var "c", Var "d")];;
let sys1 = [(Fun("f",[Var "x"]), Fun("f",[Fun("g",[Var "y"])])); (Var "y", Fun("h",[Var "p"]))];;
let sys2 = [(Fun("f",[Var "a"]), Var "b")];;
let sys3 = [Fun("f",[Var "a"; Var "b"]), Fun("f",[Var "x"; Var "y"])];;

(*
print_string (print_eq (system_to_equation sys0));;
print_string "\n";;


print_string (print_eq (system_to_equation sys1));;
print_string "\n";;

print_string (print_eq (system_to_equation sys2));;
print_string "\n";;

print_string (print_eq (system_to_equation sys3));;
print_string "\n";;
*)

(* ------------------------------ Apply substitution ------------------------------*)

module StringMap = Map.Make (String);;

let rec apply_helper_list y map = 
        let rec impl y z = match y with 
                [] -> List.rev z
                | h::t -> impl t ((apply_helper h map)::z) in
        impl y []

and apply_helper y map  = match y with
        Var a -> if StringMap.mem a map then StringMap.find a map else (Var a)
        | Fun(a, b) -> Fun(a, apply_helper_list b map);; 

let apply_substitution subst at = 
        let rec fill_map l m =
                match l with 
                        [] -> m
                        |(var, term)::t -> fill_map t (StringMap.add var term m) in

	apply_helper at (fill_map subst StringMap.empty);;


(* Test samples *)
let at0 = Fun("f",[Fun("g",[Var "y"; Var "x"])]);;

(*
print_string (type_to_string (apply_substitution ["y", Var "Alex"] at0) "");;
*)


(* ------------------------------ Check solution -------------------------------*)





let check_solution x y = failwith "Not implemented";;
















let solve_system x = failwith "Not implemented";;


