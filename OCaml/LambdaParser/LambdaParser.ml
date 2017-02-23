
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
