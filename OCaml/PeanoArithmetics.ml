
(* Created by Aleksandr Tukallo on 16.02.2017 *)

(* This code implements basic operations with
 * natural numbers in peano arithmetics.
 * Supported operations are inc, add, sub, mul, deg. *)

type peano_number = Zero
	| Prev of peano_number

let peano_from_int n =
	let rec peano_from_int_rec num peano_num =
		match num with
			0 -> peano_num
			| x -> Prev (peano_from_int_rec (x - 1) peano_num) in
	peano_from_int_rec n Zero;;

let peano_to_string peano_num =
	let rec peano_to_string_rec peano_num str =
		match peano_num with
			Zero -> "0" ^ str
			| Prev prev -> peano_to_string_rec prev "'" ^ str in
	peano_to_string_rec peano_num "";;

let inc peano_num =
	Prev peano_num;;

let rec add a b =
	match b with
		Zero -> a
		| Prev prev -> Prev(add a prev);;

let rec sub a b =
	match (a, b) with 
		(a, Zero) -> a
		| (Zero, b) -> Zero
		| (Prev a, Prev b) -> sub a b;;

let rec mul a b =
	match (a, b) with
		(b, Zero) -> Zero
		| (a, Prev b) -> add (mul a b) a;;

let rec deg a b =
	match (a, b) with
		(a, Zero) -> Prev Zero
		| (a, Prev b) -> mul (deg a b) a;;

print_string (peano_to_string (inc (peano_from_int 3))); print_string "\n";;
print_string (peano_to_string (add (peano_from_int 4) (peano_from_int 2))); print_string "\n";;
print_string (peano_to_string (sub (peano_from_int 9) (peano_from_int 8))); print_string "\n";;
print_string (peano_to_string (sub (peano_from_int 3) (peano_from_int 4))); print_string "\n";;
print_string (peano_to_string (mul (peano_from_int 2) (peano_from_int 6))); print_string "\n";;
print_string (peano_to_string (mul (peano_from_int 1) (peano_from_int 8))); print_string "\n";;
print_string (peano_to_string (deg (peano_from_int 3) (peano_from_int 3))); print_string "\n";;
print_string (peano_to_string (deg (peano_from_int 3) (peano_from_int 0))); print_string "\n";;

