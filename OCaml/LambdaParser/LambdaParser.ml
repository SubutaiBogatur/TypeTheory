
type lambda = 
	Var of string	(* just variable *)
	| Abs of string * lambda (* \x.lambda, where x is variable *)
	| App of lambda * lambda;;

(*string of lambda hb mega ezy*)
let string_of_lambda l =
	"tmp";;

(*Let variable name be 1 symbol*)
(* string -> lambda *)
let lambda_of_string s =
	let s = s ^ ";" in
	let pos = ref 0 in (*pos points to first not processed element*)
	let get () = s.[!pos] in (*returns next not processed element*)
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
				if (is_end () || s.[!pos] = ')') then ans
				else App(ans, parse_lambda ()))   
			| '(' -> 
				(eat '(';
				let ret = parse_lambda () in
				eat ')';
				if (is_end () || s.[!pos] = ')') then ret
				else App(ret, parse_lambda ()))
			| _ ->  
				(let ret = (parse_ident ()) in
				if (is_end () || s.[!pos] = ')') then ret
				else App(ret, parse_lambda ())) 

	(* unit -> lambda *)
	and parse_abs () = 
		eat '\\';
		let name = parse_ident_str () in
		eat '.';
		Abs(name, parse_lambda ()) in

	parse_lambda ();;

(*lambda_of_string ("\\x.\\y.xy");;*)	
lambda_of_string ("(x)");;
