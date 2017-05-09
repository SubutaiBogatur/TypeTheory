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

let ps sys =
	print_string (system_to_string sys "");
	print_string "\n";;


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

(* todo change strucutre *)
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

let subst_system subst sys =
	let rec impl subst sys acc =
		match sys with
			[] -> List.rev acc
			| ((l, r)::t) -> impl subst t (((apply_substitution subst l), (apply_substitution subst r))::acc) in
	impl subst sys [];;


(* Test samples *)
let at0 = Fun("f",[Fun("g",[Var "y"; Var "x"])]);;

(*
print_string (type_to_string (apply_substitution ["y", Var "Alex"] at0) "");;
*)


(* ------------------------------ Check solution -------------------------------*)

(* Function checks if two ats are equal by structure *)
(* at -> at -> bool *)
let rec eq_at at1 at2 =
	match (at1, at2) with
		(Var a, Var b) -> a = b
		| (Fun(f, l1), Fun(g, l2)) -> f = g && eq_list l1 l2 
		| _ -> false

and eq_list l1 l2 =
	match (l1, l2) with
		([], []) -> true
		| (h1::t1), (h2::t2) -> (eq_at h1 h2) && (eq_list t1 t2) 
		| _ -> false;;

(* Sorry for such names *)
let eq_eq eq = 
	match eq with
		(lhs, rhs) -> eq_at lhs rhs;;


let check_solution solution sys =
	
	let eq_eq_after_subst eq = 
		match eq with
			(lhs, rhs) -> eq_at (apply_substitution solution lhs) (apply_substitution solution rhs) in
	
	let rec impl sys =
		match sys with
			[] -> true
			| (h::t) -> (eq_eq_after_subst h) && (impl t) in
	impl sys;;

(* Test samples *)

let e1 = [(Var "a", Var "b"); (Var "x", Var "b")];;
print_string (string_of_bool (check_solution ["a", Var "b"; "x", Var "b"] e1));;



(* ------------------- Robinson algorithm: here it is -------------------------- *)

(* todo mb move inside *)
exception NoSolution of string;;

module StringSet = Set.Make (String);;

let solve_system sys =
	
	(* Checks if var is present in at *)
	let rec contains var at = 
		match at with
			(Var a) -> a = var
			| (Fun (f, l)) -> contains_l var l
	and contains_l var l =
		match l with
			[] -> false
			| (h::t) -> (contains var h) || (contains_l var t) in

	(* Function solves the system. It gets system and set of resolved vars *)
	(* list of pairs of at -> Set of strings -> list of pairs of at *)
	let rec impl sys resolved =
		if StringSet.cardinal resolved = List.length sys then sys else 	
		match sys with
			[] -> raise (NoSolution "Empty system")
			| (cur::tail) ->
				let lhs, rhs = cur in 	
				(* If right side is the same as left one just 
					remove the equation from the system *)
				if eq_at lhs rhs then impl tail resolved else
				match (lhs, rhs) with 
					(Var a, any) -> if contains a any then raise (NoSolution "Third rule abused") 
							else let resolved = StringSet.add a resolved in
							impl (List.append (subst_system [a, any] tail) [cur]) resolved 
								

	sys;;	




























