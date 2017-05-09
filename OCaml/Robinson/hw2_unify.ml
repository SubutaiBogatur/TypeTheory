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


let apply_substitution x y = failwith "Not implemented";;
let check_solution x y = failwith "Not implemented";;
let solve_system x = failwith "Not implemented";;


(* Test samples *)


let sys0 = [(Var "a", Var "b"); (Var "c", Var "d")];;
print_string (print_eq (system_to_equation sys0));;
print_string "\n";;

let sys1 = [(Fun("f",[Var "x"]), Fun("f",[Fun("g",[Var "y"])])); (Var "y", Fun("h",[Var "p"]))];;
print_string (print_eq (system_to_equation sys1));;
print_string "\n";;

let sys2 = [(Fun("f",[Var "a"]), Var "b")];;
print_string (print_eq (system_to_equation sys2));;
print_string "\n";;

let sys3 = [Fun("f",[Var "a"; Var "b"]), Fun("f",[Var "x"; Var "y"])];;
print_string (print_eq (system_to_equation sys3));;
print_string "\n";;

