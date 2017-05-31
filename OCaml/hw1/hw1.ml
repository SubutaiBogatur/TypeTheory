

type peano = Z
	| S of peano

(* --- Basic Peano operations ------- *)

let peano_of_int n =
	let rec peano_from_int_rec num peano_num =
		match num with
			0 -> peano_num
			| x -> S (peano_from_int_rec (x - 1) peano_num) in
	peano_from_int_rec n Z;;

let rec int_of_peano p = match p with
	Z -> 0
	| S x -> 1 + int_of_peano x;;

let peano_to_string peano_num =
	let rec peano_to_string_rec peano_num str =
		match peano_num with
			Z -> "0" ^ str
			| S prev -> peano_to_string_rec prev "'" ^ str in
	peano_to_string_rec peano_num "";;

let inc peano_num =
	S peano_num;;

let rec add a b =
	match b with
		Z -> a
		| S prev -> S(add a prev);;

let rec sub a b =
	match (a, b) with 
		(a, Z) -> a
		| (Z, b) -> Z
		| (S a, S b) -> sub a b;;

let rec mul a b =
	match (a, b) with
		(b, Z) -> Z
		| (a, S b) -> add (mul a b) a;;

let rec power a b =
	match (a, b) with
		(a, Z) -> S Z
		| (a, S b) -> mul (power a b) a;;


(*------------ Reverse & MergeSort -------------------------------*)



let rev l =
	let rec rev_impl old_l new_l =
		match old_l with
			[] -> new_l
			| h::t -> rev_impl t (h::new_l) in
	rev_impl l [];; 

let rec print_list_int l =
	match l with
		[] -> print_string "\n"; ()
                | h::t -> print_int h; print_list_int t;;

let merge_sort l =
	let split_in_two_equal l =
		let rec split_impl l_old l_new_1 l_new_2 =
			match l_old with
				h1::h2::t -> split_impl t (h1::l_new_1) (h2::l_new_2)
				| h1::[] -> ((h1::l_new_1), l_new_2)
				| [] -> (l_new_1, l_new_2) in
		split_impl l [] [] in

	let merge_two_sorted_lists l1 l2 =
		let rec merge_impl l1 l2 ans =
			match (l1, l2) with
				([], []) -> ans
				| ([], h::t) -> merge_impl l1 t (h::ans)
				| (h::t, []) -> merge_impl t l2 (h::ans)
				| (h1::t1, h2::t2) -> 
					if h1 < h2 then merge_impl t1 l2 (h1::ans)
						   else merge_impl l1 t2 (h2::ans) in
		let tup = merge_impl l1 l2 [] in
		rev tup in

	let rec merge_impl l =
		match l with
			[] -> l
			| h::[] -> l
			| h::t -> 
				(let tup = split_in_two_equal l in
				let (f, s) = tup in
				merge_two_sorted_lists (merge_impl f) (merge_impl s)) in
	merge_impl l;; 	


(* ---------------------------- Lambda expressions ---------------------- --*)

type lambda = Var of string | Abs of string * lambda | App of lambda * lambda


(* -------------------------------- IO & parsers ---------------------------*)

(* lambda -> string *)
let string_of_lambda l =
	let rec impl l s =
		match l with
			Var v -> s ^ v 
			| Abs (v, x) -> s ^ "(" ^ "\\" ^ v ^ "." ^ (impl x "") ^ ")" 
			| App (x, y) -> s ^ "(" ^ (impl x "") ^ " " ^ (impl y "") ^ ")" in
	impl l "";;

(* Parser of lambda expressions for grammar :
	<lambda> -> <expr> ' ' <abs> | <expr> | <abs>
	<abs> -> \<var>.<lambda>
	<expr> -> { <var> | (<lambda>) }+{' '} *)
(* string -> lambda *)
let lambda_of_string s =
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
				then 
					let c = get() in
					next();
					impl (s ^ String.make 1 c)
				else s in
		impl "" in	

	(* unit -> lambda *)
	let parse_name () = 
		Var(parse_name_str ()) in

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
				(let ans = (parse_name ()) in
				if_is_app ans)

	(* unit -> lambda *)
	and parse_abs () = 
		eat '\\';
		let name = parse_name_str () in
		eat '.';
		Abs(name, parse_lambda ())

	(* function checks if expression continues *)
	(* lambda -> lambda *)	
	and if_is_app prev = 
		if (is_end () || s.[!pos] = ')') then prev 
		else    (eat ' '; 
	        	App(prev, parse_lambda ())) in 

	parse_lambda ();;

(*
print_string (string_of_lambda (lambda_of_string "(x)")); print_string "\n";; 
print_string (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))")); print_string "\n";; 
print_string (string_of_lambda (lambda_of_string "((z)) (\\x.\\y.((x y)))")); print_string "\n";;
*)
print_string (string_of_lambda (lambda_of_string "\\x.x (y z)")); print_string "\n";;
