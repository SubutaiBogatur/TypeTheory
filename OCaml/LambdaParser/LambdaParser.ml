
(* Function as marker for debug *)
let d () =
	false;;

type lambda = 
	Var of string	(* just variable *)
	| Abs of string * lambda (* \x.lambda, where x is variable *)
	| App of lambda * lambda;;

(* lambda -> string *)
let string_of_lambda l =
	let rec impl l s =
		match l with
			Var v -> s ^ "(" ^  v ^ ")"
			| Abs (v, x) -> s ^ "(" ^ "\\" ^ v ^ "." ^ (impl x "") ^ ")" 
			| App (x, y) -> s ^ "(" ^ (impl x "") ^ (impl y "") ^ ")" in
	impl l "";;

(* Let variable have name of grammar [a...z]{[0...9]}* *)
(* string -> lambda *)
let lambda_of_string s =
	let s = s ^ ";" in
	let pos = ref 0 in (*pos points to first not processed element*)
	let get () = s.[!pos] in (*ansurns next not processed element*)
	let next () = if !pos < String.length s - 1 then pos := !pos + 1 in (*increment pos*)
	let eat x = if get () = x then next () else failwith "Incorrect input string" in
	let is_end () = if (get ()) = ';' then true else false in		
		
	(* unit -> string *)
	let parse_ident_str () =
		let rec digits_adder s =
			if (get ()) >= '0' && (get ()) <= '9' 
				then 
					let c = get() in
					next();
					digits_adder (s ^ String.make 1 c)
				else s in
		let var = String.make 1 (get ()) in
		next ();
		digits_adder var in	

	(* unit -> lambda *)
	let parse_ident () = 
		Var(parse_ident_str ()) in

	(* unit -> lambda *)
	let rec parse_lambda () =
		match (get ()) with 
			'\\' -> 
				(let ans = parse_abs () in
				if_is_app ans)
			| '(' -> 
				(eat '(';
				let ans = parse_lambda () in
				eat ')'; 
				if_is_app ans)
			| _ ->  
				(let ans = (parse_ident ()) in
				if_is_app ans)

	(* unit -> lambda *)
	and parse_abs () = 
		eat '\\';
		let name = parse_ident_str () in
		eat '.';
		Abs(name, parse_lambda ())

	(* function checks if expression continues *)
	(* lambda -> lambda *)	
	and if_is_app prev = 
		if (is_end () || s.[!pos] = ')') then prev 
		else App(prev, parse_lambda ()) in 

	parse_lambda ();;

(* lambda -> lambda -> bool *)
let is_alpha_equivalent x y =
	let subst_counter = ref 0 in
	
	(* Function generates next name for substitution *)	
	(* unit -> string *)
	let next_name_generator () =
		let res = "t" ^ string_of_int !subst_counter in
		subst_counter := !subst_counter + 1; 
		res in

	(* Function substitutes v_new in all !free! occurences of v_old in lambda *)
	(* lambda -> string -> string -> lambda *)
	let rec subst l v_old v_new =
		if d () then print_string ("subst: " ^	(string_of_lambda l) 
				^ " " ^ v_old ^ " " ^ v_new ^ "\n"); 
		match l with 
			Var v -> if v = v_old then Var v_new else Var v
			| Abs (v, x) -> 
				(* If v_old is enslaved by lambda, no more subst *)
				if v = v_old then l else Abs (v, subst x v_old v_new)
			| App (x, y) ->
				App (subst x v_old v_new, subst y v_old v_new) in

	let rec impl x y =
		match (x, y) with
			(Var a, Var b) -> a = b (* Check if same vars *)
			| (App (p, q), App(r, s)) ->
				(impl p r) && (impl q s)	
			| (Abs (v1, p), Abs (v2, q)) ->
				let v = (next_name_generator ()) in
				impl (subst p v1 v) (subst q v2 v)
			| _ -> false in
	impl x y;; 

let rec print_string_list l =
	match l with
		[] -> ()
		| h::t -> print_string h; print_string_list t;; 

module StringSet = Set.Make (String);;

(* Functions checks if theta is free for substitution instead of var in alpha *)
(* lambda -> lambda -> string -> bool *)
let is_free_for_subst theta alpha var =
	
	(* Function returns set with all free (ie not under lambdas) vars in l *)
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
				StringSet.union (all_free_vars x blocked) (all_free_vars y blocked) in
	all_free_vars theta StringSet.empty;;

(*
let theta = lambda_of_string "(x)(y)(\\x.x)(\\z.z)(\\w.\\v.u)(\\a.b)";;
print_string_list (StringSet.elements (is_free_for_subst theta (Var "x") "x"));;
*)









