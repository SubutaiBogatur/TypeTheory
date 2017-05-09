type algebraic_term = Var of string | Fun of string * (algebraic_term list)


(* ---------------------------- Util functions ----------------------------- *)

let term_to_string at =
	
	let rec impl at =
		match at with
			Var v -> v
			| Fun(f, args) -> f ^ "(" ^ (impl_l args) ^ ")"
	and impl_l l =
		match l with
			[] -> "" 
			| (h::[]) -> (impl h) (* for last arg no last space *)
			| (h::t) -> (impl h) ^ " " ^ (impl_l t) in
	
	impl at;;

let equation_to_string eq =
	let lhs, rhs = eq in
	term_to_string lhs ^ " = " ^ term_to_string rhs;;


let rec system_to_string sys =
	match sys with
		[] -> ""
		| (h::t) -> (equation_to_string h) ^ "\n" ^ (system_to_string t);;			

(* Println algebraic term *)
let pat at = 
	print_string (term_to_string at);
	print_string "\n";;

(* Println equation *)
let peq eq =
	print_string (equation_to_string eq);
	print_string "\n";;

(* Println system *)
let psys sys =
	print_string (system_to_string sys);;
	
(* Function gets str and at and checks if str is met in at.
	If it is not met 0 is returned.
	If it is met only as variable 1 is returned.
	If it is met only as function 2 is returned.
	If it is met both as var and fun 3 is retuned.
	Consider using not this function, but mem functions instead*)
(* string -> at -> int *)
let rec contains str at msk = 
	match at with
		(Var a) -> if a = str then msk lor 1 else msk 
		| (Fun (f, l)) -> (contains_l str l msk) lor (if str = f then 2 else 0)
and contains_l str l msk =
	match l with
		[] -> msk
		| (h::t) -> (contains str h msk) lor (contains_l str t msk);; 

(* Mem functions' names come from mem function in Set and Map modules *)
(* string -> at -> bool *)
let mem str at =
	contains str at 0 <> 0;;

let memf str at =
	contains str at 0 land 2 <> 0;;

let memv str at =
	contains str at 0 land 1 <> 0;;

(* Test samples *)
(* Here are some basic examples to do tests on *)

let sys0 = [(Var "a", Var "b"); (Var "c", Var "d")];;
let sys1 = [(Fun("f",[Var "x"]), Fun("f",[Fun("g",[Var "y"])])); (Var "y", Fun("h",[Var "p"]))];;
let sys2 = [(Fun("f",[Var "a"]), Var "b")];;
let sys3 = [Fun("f",[Var "a"; Var "b"]), Fun("f",[Var "x"; Var "y"])];;

let at0 = Var "a";;
let at1 = Var "b";;
let at2 = Var "c";;
let at3 = Var "d";;
let at4 = Fun("f",[Var "x"]);;
let at5 = Fun("f",[Fun("g",[Var "y"])]);;
let at6 = Fun("h",[Var "p"]);;
let at7 = Fun("f",[Var "a"; Var "b"]);;
let at8 = Fun("f",[Var "x"; Var "y"]);;
let at9 = Fun("f",[at5; at7]);;

(* Print bool *)
let pb b =
	print_string (string_of_bool b);
	print_string "\n";;


(* ------------------------ System to Equation ------------------------------ *)

(* Function gets system of equation as an input and returns
	function name that is never met in the system. The name
	is generated as concatenation of all names of 
	function. Maybe implementation should be changed *)
let get_fresh_fun_name system =

	let rec mem_sys str sys =
		
		let mem_eq str eq =
			let l, r = eq in
			(mem str l) || (mem str r) in

		match sys with
			[] -> false
			| (h::t) -> (mem_eq str h) || (mem_sys str t) in 
	
	let rec impl str system counter =
		if (mem_sys (str ^ string_of_int counter) system) 
		then impl str system (counter + 1) else (str ^ string_of_int counter) in

	impl "fresh" system 1;;

let rec eq_helper x ll rr = match x with 
        |[] -> ll, rr
        |(lh, rh)::t -> eq_helper t (lh::ll) (rh::rr);; 

let system_to_equation x = 
        let l, r = eq_helper x [] [] in
        let fresh_name = get_fresh_fun_name x in
	(Fun(fresh_name, l), Fun(fresh_name, r));;

peq (system_to_equation sys3);;


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
(*
(* todo mb move inside *)
exception NoSolution of string;;

module StringSet = Set.Make (String);;

let solve_system sys =
	

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



*)
























