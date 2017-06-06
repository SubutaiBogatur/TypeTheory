

let ps s =
	print_string s;
	print_string "\n";;


(* Function is almost fully copypasted from Hw1 module,
	where we were parsing primitive lambdas from string.
	To get rid of copying the one should probably
	use generic typic or templates and I'm 
	not ready to do it yet 

	This function is implemented only because of 
	enthusiasm and just for fun. It is not 
	homework. As a result it is prohibited to
	start variable names with "i" or with "l" in
	strings passed to this function (for easier "let .. in .." parsing)
	
	Consider yourself warned *)

let hml_of_string s =
	let s = s ^ ";" in
	let pos = ref 0 in (*pos points to first not processed element*)
	let get () = s.[!pos] in (*returns next not processed element*)
	let next () = if !pos < String.length s - 1 then pos := !pos + 1 in (*increment pos*)
	let eat x = if get () = x then next () else failwith "Incorrect input string" in
	let is_end () = if (get ()) = ';' then true else false in		
		
	(* Function reads the name from current position till next space or dot *)
	(* unit -> string *)
	let parse_name_str () =
		let rec impl s =
			if (get ()) <> ' ' && (get ()) <> '.' && (get ()) <> ')' && not (is_end ())  
				then let c = get() in next(); impl (s ^ String.make 1 c)
				else s in
		impl "" in	

	(* unit -> hml *)
	let parse_name () = 
		Hw2_inference.HM_Var(parse_name_str ()) in

	(* unit -> hml *)
	let rec parse_hml () =
		let ans = 	
			match (get ()) with 
				'\\' -> parse_abs ()
				| '(' -> (eat '('; let ret = parse_hml () in eat ')'; ret)
				| 'l' -> parse_let () (* todo fix for vars starting with l *)
				| _ ->  parse_name () in
		if_is_app ans 

	and parse_let () =
		eat 'l'; eat 'e'; eat 't'; eat ' ';
		let v = parse_name_str () in
		eat ' '; eat '='; eat ' ';
		let hml1 = parse_hml () in
		eat 'i'; eat 'n'; eat ' '; (* space before in is read by app *)
		let hml2 = parse_hml () in
		Hw2_inference.HM_Let(v, hml1, hml2) 

	(* unit -> lambda *)
	and parse_abs () = 
		eat '\\';
		let name = parse_name_str () in
		eat '.';
		Hw2_inference.HM_Abs(name, parse_hml ())

	(* function checks if expression continues and makes app left associative *)
	(* lambda -> lambda *)	
	and if_is_app prev = 
		if (is_end () || s.[!pos] = ')' || s.[!pos] = 'i') then prev else 
			(eat ' '; 
			(* if we encounter 'i' after space, than it is beginning of "in", no app here *)
			if (get ()) = 'i' then prev else
				if_is_app (Hw2_inference.HM_App(prev, 
					match (get ()) with 
						'\\' -> parse_abs () 
						| '(' -> (eat '('; let ans = parse_hml () in	eat ')'; ans)
						| 'l' -> parse_let () 
						| _ -> parse_name ()))) in

	parse_hml ();; 


let test str =
	ps ("Testing:        " ^ str);
	let hml = hml_of_string str in
	ps ("Parsed version: " ^ (Hw2_inference.string_of_hml hml));
	match (Hw2_inference.algorithm_w hml) with
		None -> ps "Unable to infer type with W"
		| Some (cxt, hmt) -> 
			(ps ("Infered type: " ^ (Hw2_inference.string_of_hmt hmt));
			List.iter (fun (v, hmt) -> ps ("{" ^ v ^ " : " ^ (Hw2_inference.string_of_hmt hmt) ^ "}")) cxt;
			ps "\n");; 


let t1t = "let id = \\x.x in \\f.\\x.id (id (id x))";; 
let t2t = "let id = \\x.x in \\f.\\x.id f (id (id x))";; 
let t3f = "let id = \\x.x in \\f.\\x.id f (id x (id x))";; 
let t4t = "let id = \\t.t in \\f.\\x.(id f) (id x)";;

test t1t;;



