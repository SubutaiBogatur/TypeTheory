
(* Created by Aleksandr Tukallo on 16.02.2017 *)

(* This code implements basic operations with
 * natural numbers in peano arithmetics.
 * Supported operations are inc, add, sub, mul, deg. *)

type peano = Z
	| S of peano

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
