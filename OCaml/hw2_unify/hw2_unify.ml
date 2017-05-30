
(* Created by Aleksandr Tukallo on 9'th of May *)


type algebraic_term = Var of string | Fun of string * (algebraic_term list)
type at = algebraic_term
type eq = (at * at) (* pair of lhs and rhs *) 
type sys = eq list (* system is a list of equations *)

(* ---------------------------- Util functions ----------------------------- *)

(* at -> string *)
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


(* eq -> string *)
let equation_to_string eq =
	let lhs, rhs = eq in
	term_to_string lhs ^ " = " ^ term_to_string rhs;;


(* sys -> string *)
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
	print_string (system_to_string sys);
	print_string "\n";;
	
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


(* Function for checking structure equivalence *)
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

(* Sorry for such names: checks if lhs equals rhs by structure *)
(* eq -> bool *)
let eq_eq eq = 
	match eq with
		(lhs, rhs) -> eq_at lhs rhs;;

(* Test samples *)
(* Here are some basic examples to do tests on *)

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

let sys0 = [(Var "a", Var "b"); (Var "c", Var "d")];;
let sys1 = [(Fun("f",[Var "x"]), Fun("f",[Fun("g",[Var "y"])])); (Var "y", Fun("h",[Var "p"]))];;
let sys2 = [(Fun("f",[Var "a"]), Var "b")];;
let sys3 = [Fun("f",[Var "a"; Var "b"]), Fun("f",[Var "x"; Var "y"])];;
let sys4 = List.append sys1 [(at7, at2); (at7, at2)];;

let isys0 = [at4, at8];;
let isys1 = [Fun("f",[Var "y"; Fun("h",[Var "x"; Var "y"])]), Fun("f",[Fun("g",[Var "a"; Var "b"]); Fun("h", [Var "x"; Var "x"])]); Fun("h", [Var "x"; Var "y"]), Fun("h", [Var "a"; Var "a"])];;



(* Print bool *)
let pb b =
	print_string (string_of_bool b);
	print_string "\n";;


(* ------------------------ System to Equation ------------------------------ *)

(* Function gets system of equations as an input and returns
	name that is never met in the system. *) 
(* sys -> string *)
let get_fresh_name system =

	(* string -> sys -> bool *)
	let rec mem_sys str sys =
		(* string -> eq -> bool *)
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

let system_to_equation sys = 

	(* Function separates system in two lists of equations:
		list of lhs and list of rhs *)
	(* sys -> at list -> at list -> (at list * at list) *)
	let rec separator sys lhsl rhsl : (at list * at list) = match sys with 
       		 [] -> List.rev lhsl, List.rev rhsl
        	 | (lh, rh)::t -> separator t (lh::lhsl) (rh::rhsl) in
        let l, r = separator sys [] [] in
        let fresh_name = get_fresh_name sys in
	(Fun(fresh_name, l), Fun(fresh_name, r));;

(* peq (system_to_equation [system_to_equation sys1; (at1, at2)]);; *)


(* ------------------------------ Apply substitution ------------------------------*)

module StringMap = Map.Make (String);;

(* Function gets substitution and applies it. Substitution is list of
	variable names and algebraic terms, to which that vars must be changed *)
(* (string * algebraic_term) list -> at -> at *)
let apply_substitution subst at = 

	(* Function walks the at and makes the substitutions *)
	let rec impl_at at map  = match at with
		Var a -> if StringMap.mem a map then StringMap.find a map else (Var a)
		| Fun(a, b) -> Fun(a, impl_list b map)

	and impl_list l map = 
		let rec impl l ans = match l with 
			[] -> List.rev ans 
			| h::t -> impl t ((impl_at h map)::ans) in
		impl l [] in

        let rec fill_map subst m =
                match subst with 
                        [] -> m
                        | (var, term)::t -> fill_map t (StringMap.add var term m) in

	impl_at at (fill_map subst StringMap.empty);;

(* Apply substitution in a system *)
(* (string * at) list -> sys -> sys *)
let apply_substitution_sys subst sys =
	let rec impl subst sys acc =
		match sys with
			[] -> List.rev acc
			| ((l, r)::t) -> impl subst t (((apply_substitution subst l), (apply_substitution subst r))::acc) in
	impl subst sys [];;


(* ------------------------------ Check solution -------------------------------*)


let check_solution solution sys =
	(* Function does subst in lhs and rhs and then checks their structural equivalence *)
	let eq_eq_after_subst subst eq = 
		match eq with
			(lhs, rhs) -> eq_at (apply_substitution subst lhs) (apply_substitution subst rhs) in
	let rec impl subst sys =
		match sys with
			[] -> true
			| (h::t) -> (eq_eq_after_subst subst h) && (impl subst t) in
	impl solution sys;;


(* ------------------- Robinson algorithm: here it is -------------------------- *)

exception NoSolution of string;;

module StringSet = Set.Make (String);;

let solve_system sys =

	(* Function builds a system for arguments. The system will look like [l1[i]=l2[i];...] *)
	(* at list -> at list -> sys *)
	let get_args_sys l1 l2 =
		let rec impl l1 l2 ans =
			match (l1, l2) with 
				([], []) -> List.rev ans (* it looks like rev is not needed *)
				| (h1::t1, h2::t2) -> impl t1 t2 ((h1, h2)::ans) 
				| _ -> failwith "never will happen, just to get rid of warning" in
		impl l1 l2 [] in
		

	(* Function solves the system. It gets system and set of resolved vars *)
	(* sys -> StringSet -> sys *)
	let rec impl sys resolved =
		psys sys;
		if StringSet.cardinal resolved = List.length sys then sys else (* If all eqs are resolved return resolved system*)	
		match sys with
			[] -> raise (NoSolution "Empty system")
			| (lhs, rhs)::tail ->
				let cur = lhs, rhs in
				if eq_at lhs rhs then impl tail resolved else (* If (eq rhs lhs) then  just remove the equation from the system *)
				match (lhs, rhs) with 
					Var a, any -> if memv a any then raise (NoSolution "Fourth rule abused") (* Variable a is met in rhs *)
							else let resolved = StringSet.add a resolved in
							(* todo: mb subst if not was resolved already *)
							impl (List.append (apply_substitution_sys [a, any] tail) [cur]) resolved 
					| any, Var a -> impl (List.append tail [rhs, lhs]) resolved 
					| Fun(f, l1), Fun(g, l2) -> if f <> g || List.length l1 <> List.length l2 then raise (NoSolution "Third rule abused")
									else impl (List.append tail (get_args_sys l1 l2)) resolved in
	(* Function converts system to needed return type *)
	(* sys -> (string * at) list *)
	let dewrap sys =
		let rec impl sys ans =
			match sys with 
				[] -> List.rev ans
				| ((Var a, rhs)::tail) -> 
					impl tail ((a, rhs)::ans) 
				| _ -> failwith "it's impossible, sorry" in
		impl sys [] in

	try 
		let resolved_system = impl sys StringSet.empty in
		print_string "Answer: \n";
		psys resolved_system;
		(Some (dewrap resolved_system))

	with (NoSolution msg) -> print_string msg;
				print_string "\n";
				None;;
				

	

