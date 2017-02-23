
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

(* Let variable name be 1 symbol *)
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
		let var = String.make 1 (get ()) in
		next();
		var in 	

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

(* Function substitutes v_new in all !free! occurences of v_old in lambda *)
(* lambda -> string -> string -> lambda *)
let rec subst l v_old v_new =
	match l with 
		Var v -> if v = v_old then Var v_new else Var v
		| Abs (v, x) -> 
			(* If v_old is enslaved by lambda, it's not free, no more subst *)
			if v = v_old then l else Abs (v, subst x v_old v_new)
		| App (x, y) ->
			App (subst x v_old v_new, subst y v_old v_new);;
	

(* lambda -> lambda -> bool *)
let rec is_alpha_equivalent x y =
	match (x, y) with
		(Var a, Var b) -> a = b (* Check if same vars *)
		| (App (p, q), App(r, s)) ->
			(is_alpha_equivalent p r) && (is_alpha_equivalent q s)	
		| (Abs (v1, p), Abs (v2, q)) ->
			let v = "t" in (* Todo function to generate them *)
			is_alpha_equivalent (subst p v1 v) (subst q v2 v)
		| _ -> false;; 





